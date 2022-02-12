import os
import requests
import json
from datetime import datetime

repo_id="MDEwOlJlcG9zaXRvcnkyMjg3NDM0MjQ="
category_id="DIC_kwDODaJZAM4CA7bf"
DISCUSSIONS_TOKEN=os.environ.get('DISCUSSIONS_TOKEN')

path = os.getcwd()
currentYear = str(datetime.now().year)

filelist = []
postlist = []

for root, dirs, files in os.walk(path + '/source'):
    for name in files :
        filelist.append(os.path.join(root, name))

strset = set([])
for filename in  filelist:
    if filename.endswith('.md'):
        if "_posts/" in filename:
            post = filename.split("_posts/")[-1]
            if currentYear in post:
                postlist.append(post[:-3] + '/')

        with open(filename, "r",encoding='utf-8') as f:
            for subset in f.read():
                strset.add(subset)

str_ = ''
a = 0
with open('strdb.txt', "w") as f:
    for i in list(strset):
        str_ += i
        a += 1
    print('%d Characters in all files.'%a)
    print(str_)
    f.write(str_)


header = {
    "Authorization": f"Bearer {DISCUSSIONS_TOKEN}"
}
data = {
    'query': """query{repository(owner:"southfox-d",name:"southfox-d.github.io"){
            id discussions(first:100, categoryId: "DIC_kwDODaJZAM4CA7bf"){
                nodes{
                    title
                }
            }
        }
    }"""
}
url = 'https://api.github.com/graphql'

r = requests.post(url, headers=header, data=json.dumps(data))
if r.status_code == 200:
    discussions = r.json()

discussions = discussions["data"]["repository"]["discussions"]["nodes"]

discu_list = []
for i in discussions:
    discu_list.append(i["title"])

for post in postlist:

    if post not in discu_list:
        data = {
        'query': """mutation{
        createDiscussion(input: {repositoryId: "%s", categoryId: "%s", body: "%s", title: "%s"}) {
            discussion {
                id
                    }
                }
            }""" % (repo_id, category_id, 'https://blog.southfox.me/' + post, post)
        }

        r = requests.post(url, headers=header, data=json.dumps(data))
        if r.status_code == 200:
            print(f"初始化对应讨论！ {post}")
            print(r.text)
