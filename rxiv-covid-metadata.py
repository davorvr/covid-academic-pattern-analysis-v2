#!/usr/bin/python3
from urllib.request import urlopen
import json
import datetime
import time
import dateutil.parser as dparser
import selenium
from selenium import webdriver

## Dependencies as pip packages:
# urllib3
# python-dateutil
# selenium

## find_date function
# First searches for the keyword "x" in webpage source ("s"), and then looks for
# a year (1900-2100) within the next 50 characters. This segment is then passed to
# dateutil.parser with fuzzy matching to extract a date which it returns in
# ISO 8601-compliant format. Returns None otherwise.
def find_date(s, x):
    years = [ str(x) for x in range(1900,2100) ]
    #num = [ str(x) for x in range(0,10) ]
    x = x[1:]
    try:
        i = s.index(x)+len(x)
        i_end = i
        while (s[i_end-4:i_end] not in years) and (i_end-i) < 50:
            i_end += 1
    except (IndexError, ValueError):
        return None
    try:
        out = dparser.parse(s[i:i_end], fuzzy=True).strftime("%Y-%m-%d")
    except dparser.ParserError:
        return None
    else:
        return out

gecko = webdriver.Firefox()

## Opens the json of the "COVID-19 SARS-CoV-2 preprints from medRxiv and bioRxiv" collection.
## Collection link: https://connect.biorxiv.org/relate/content/181
collection_url = "https://connect.biorxiv.org/relate/collection_json.php?grp=181"
with urlopen(collection_url) as url:
    rxiv_articlelist = json.loads(url.read().decode()).get("rels")

# Template in dict format for a single article's metadata.
article_template = {
    "title": "",
    "doi": "",
    "rxiv_url": "",
    "rxiv_date": "",
    "rxiv_site": "",
    "affs": [],
    "journal": None,
    "journal_doi": None,
    "journal_date_received": None,
    "journal_date_accepted": None,
    "journal_date_published": None,
    "journal_url": None
}

## Template for the metadata of a single author. One list of multiple author dicts
## corresponds to one article dict. "affs_i" contains indexes of the "affs" list
## in the corresponding article dict (index origin = 0). For further clarification
## and an example on how to access the affiliations, see below.
author_template = {
    "name": "",
    "affs_i": []
}

## The following lists will contain the final output. Every element of article_list is one
## article dict, and every element of author_list is one list of author dicts. The
## name of "author_list" is thus inaccurate - it ought to be more precisely called
## "author_list_list", as it is a list of author lists, each corresponding to one article.
## This means that the index of an article corresponds to the index of its author list.
#####
## An example of working with these two structures is as follows: After running
## the script, the following code prints affiliations of the third author (index 2)
## of article 428 (list index 427):
#
#article_affiliations_list = article_list[427].get("affs")
#for i in author_list[427][2].get("affs_i"):
#    print(article_affiliations_list[i])
#
#####
article_list = [None] * len(rxiv_articlelist)
author_list = [None] * len(rxiv_articlelist)

for i in range(len(rxiv_articlelist)):
    ## Fetch the next article...
    rxiv_json = rxiv_articlelist.pop()
    ## ...and a fresh metadata template.
    article = article_template.copy()
    
    ## This data is retrieved directly from the rxiv collection json.
    article["title"] = rxiv_json.get("rel_title")
    article["doi"] = rxiv_json.get("rel_doi")
    article["rxiv_url"] = rxiv_json.get("rel_link")
    article["rxiv_date"] = rxiv_json.get("rel_date")
    article["rxiv_site"] = rxiv_json.get("rel_site")
    
    ## Article affiliations are retrieved from the regular article rxiv URL.
    gecko.get(str(article.get("rxiv_url"))+".article-info")
    affs = gecko.find_elements_by_css_selector("li.aff")
    affs = [ x.text.lstrip("0123456789") for x in affs ]
    article["affs"] = affs
    
    ## Authors are retrieved from the regular article rxiv site as well.
    article_authors = []
    authors_rxiv_css = gecko.find_elements_by_css_selector("li.contributor")
    for a_in in range(len(authors_rxiv_css)):
        author = author_template.copy()
        try:
            author["name"] = authors_rxiv_css[a_in].find_element_by_css_selector("span.name").text
        except selenium.common.exceptions.NoSuchElementException:
            try:
                author["name"] = authors_rxiv_css[a_in].find_element_by_css_selector("span.name").text
            except selenium.common.exceptions.NoSuchElementException:
                ## Sometimes the author doesn't have a name, but is rather a contributing
                ## organization or similar.
                author["name"] = authors_rxiv_css[a_in].text
        affs_i_css = authors_rxiv_css[a_in].find_elements_by_css_selector("a.xref-aff")
        try:
            affs_i = [ int(x.text)-1 for x in affs_i_css ]
        except ValueError:
            ## Sometimes affiliations aren't stated for all authors. Usually, this is the case
            ## when there is only one affiliation that applies to all authors.
            affs_i = [ 0 ]
        author["affs_i"] = affs_i
        article_authors.append(author)
    
    ## The "pub_jnl" class in the rxiv webpage source contains info on whether the article
    ## is published elsewhere.
    pub_jnl = None
    for retry_pub_jnl in range(5):
        ## Sometimes, the pub_jnl element doesn't load properly.
        ## We retry five times with 5-second breaks in between.
        try:
            pub_jnl = gecko.find_element_by_class_name("pub_jnl").text
        except:
            gecko.refresh()
            time.sleep(5)
        else:
            break
    
    ## If the article is published in a journal, we try to extract some more specific data.
    if "Now published in" in pub_jnl:
        pub_jnl = pub_jnl.split(" ")
        article["journal"] = " ".join(pub_jnl[3:pub_jnl.index("doi:")])
        article["journal_doi"] = pub_jnl[-1]
        gecko.get("https://dx.doi.org/"+str(article["journal_doi"]))
        time.sleep(1)
        article["journal_url"] = gecko.current_url
        
        source = gecko.page_source
        
        ## A temporary dict for dates.
        j_dates = {
            "journal_date_received": None,
            "journal_date_accepted": None,
            "journal_date_published": None
        }
        
        ## We try to find the relevant publication dates on the journal webpage.
        for retry_date in range(2):
            for key in j_dates:
                if not j_dates[key]:
                    j_dates[key] = find_date(source, key)
            if None not in j_dates.values():
                ## If all dates are found, great. Break out of the loop.
                break
            
            ## Otherwise, we try clicking on an "Info" button...
            info_elems = gecko.find_elements_by_partial_link_text("Info")
            if not info_elems:
                break
            else:
                for info in info_elems:
                    for retry_click in range(2):
                        ## ...twice, in case the first time doesn't do it. This usually
                        ## happens in case of cookie policy pop-ups or similar.
                        try:
                            info.click()
                        except:
                            pass
                        else:
                            break
                    time.sleep(0.5)
        
        ## We copy what we got into the article's main dict.
        for key in j_dates:
            article[key] = j_dates[key]
    
    ## We got what we could, so the article is added to the complete list...
    article_list[i] = article
    ## ...along with its author data. Notice the two lists use the same index
    ## for an article's data, semantically linking them together.
    author_list[i] = article_authors

## The lists are dumped into their respective json files.
with open("author_list.json", "w") as f: 
    json.dump(author_list, f)

with open("article_list.json", "w") as f:
    json.dump(article_list, f)
