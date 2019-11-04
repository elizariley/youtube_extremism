import os # operating systems package
import pandas as pd # data analysis package

df = pd.read_csv('/Users/elizariley/Desktop/FEMALES/yt_pages_female.csv') # csv of female YT pages 

# define download_subs function, which uses youtube-dl
def download_subs(video_url, lang="en"):
    cmd = [
        "youtube-dl",
        "-i", # ignore errors
        "--skip-download",
        "--write-auto-sub",
        "--sub-lang",
        lang,
        video_url
    ]

    os.system(" ".join(cmd))

for i in range(len(df)):
    page = df['name']
    os.system("mkdir " + page[i]) # new folder for each name
    os.chdir("/Users/elizariley/Desktop/FEMALES/" + page[i])
    print(os.getcwd()) # to check that this is working as intended

    url = df['page_url']
    download_subs(url[i]) # scrape vids for each page in said folder
    os.chdir("..") # move to parent directory after downloading videos into new folder


# This script takes in a csv which has two columns: name of altR YouTuber, and the associated page url (which YouTube page contains multiple videos)
# This script ignores fatal errors and skips to the next video within a page
# This script creates a new folder (which folder is named after the respective altRighter)
# This goes through and creates a new folder for each altRighter, scrapes all of the videos for the altRighter (simply moving beyond the errors to the next vid),
# and then goes to the next and does this for all of the altRighters.
# The end product of from this script is different folders for each altRighter each with scraped video text as .vtt files, for all available videos.
