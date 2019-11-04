# This script creates json files for each video with the following video information: title, channel name, number of views, likes, dislikes, number of subscriptions, and hashtags.

import urllib.request
import urllib.parse
import urllib.error
from bs4 import BeautifulSoup
import ssl
import json
import ast
import json
import os
from urllib.request import Request, urlopen
import pandas as pd

# read id csv file in
df = pd.read_csv('female_vtts_IDs_combined.csv')

# For ignoring SSL certificate errors
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE
counter = 1 #add the number to json file name
# Input from user
for i in range(len(df)):
    v = df['ID']
    url = "https://www.youtube.com/watch?v=" + v[i] # for each vid ID

    # Making the website believe that you are accessing it using a mozilla browser

    req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    webpage = urlopen(req).read()

    # Creating a BeautifulSoup object of the html page for easy extraction of data.

    soup = BeautifulSoup(webpage, 'html.parser')
    html = soup.prettify('utf-8')
    video_details = {}
    other_details = {}

    for span in soup.findAll('span',attrs={'class': 'watch-title'}):
        video_details['TITLE'] = span.text.strip()

    for script in soup.findAll('script',attrs={'type': 'application/ld+json'}):
            channelDesctiption = json.loads(script.text.strip())
            video_details['CHANNEL_NAME'] = channelDesctiption['itemListElement'][0]['item']['name']

    for div in soup.findAll('div',attrs={'class': 'watch-view-count'}):
        video_details['NUMBER_OF_VIEWS'] = div.text.strip()

    for button in soup.findAll('button',attrs={'title': 'I like this'}):
        video_details['LIKES'] = button.text.strip()

    for button in soup.findAll('button',attrs={'title': 'I dislike this'}):
        video_details['DISLIKES'] = button.text.strip()

    for span in soup.findAll('span',attrs={'class': 'yt-subscription-button-subscriber-count-branded-horizontal yt-subscriber-count'}):
        video_details['NUMBER_OF_SUBSCRIPTIONS'] = span.text.strip()

    hashtags = []
    for span in soup.findAll('span',attrs={'class': 'standalone-collection-badge-renderer-text'}):
        for a in span.findAll('a',attrs={'class': 'yt-uix-sessionlink'}):
            hashtags.append(a.text.strip())
    video_details['HASH_TAGS'] = hashtags

    with open('output_file.html', 'wb') as file:
        file.write(html)
    #create a new file data1.json (or whatever the counter is)
    with open('data' + str(counter) + '.json', 'w', encoding='utf8') as outfile: # look into renaming json files to yt vid title
        json.dump(video_details, outfile, ensure_ascii=False,indent=4)
    counter += 1 #increase counter aka number each outputted file name

    print ('----------Extraction of data is complete. Check json file.----------')
  
