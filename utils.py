import sys
import os
import requests
import json
import getopt
import re
import random
from datetime import datetime



def fontChar():
    path = os.getcwd()
    for root, dirs, files in os.walk(path + '/source'):
        for name in files :
            filelist.append(os.path.join(root, name))

    strset = set([])
    for filename in  filelist:
        if filename.endswith('.md'):
            if "_posts/" in filename:
                post = filename.split("_posts/")[-1]
                postlist.append(post[:-3] + '/')

            with open(filename, "r", encoding='utf-8') as f:
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

def addDiscussion():
    currentYear = str(datetime.now().year)
    repo_id="MDEwOlJlcG9zaXRvcnkyMjg3NDM0MjQ="
    category_id="DIC_kwDODaJZAM4CA7bf"
    DISCUSSIONS_TOKEN=os.environ.get('DISCUSSIONS_TOKEN')

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
            if currentYear not in post:
                continue

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
                    print(f"==========\n初始化对应讨论！ {post}\n==========")
    else:
        print("错误！")

def backupImg():
    path = os.getcwd()
    for root, dirs, files in os.walk(path + '/source/_posts'):
        for name in files :
            filelist.append(os.path.join(root, name))

    nowImgList = []
    for filename in  filelist:
        if filename.endswith('.md'):
            if "_posts/" in filename:
                post = filename.split("_posts/")[-1]

                with open(filename, "r", encoding='utf-8') as f:
                    mdText = f.read()
                    result = re.findall('!\[(.*?)\]\((.*?)\)', mdText)

                    for i in range(len(result)):
                        #img_quote = result[i][0]
                        img_url = result[i][1]
                        if 'ipfs' in img_url:
                            urlname = img_url.split(u"/")
                            img_url = str(urlname[-1])
                        nowImgList.append(img_url)

    downLoadList = list(set(nowImgList).difference((set(imgJson["img"]))))
    if downLoadList != '':
        # os.environ['NEW_IMG']="True"

        if os.path.exists('./newimg'):
            pass
        else:
            os.makedirs('./newimg')

        nowImgJson = {}
        nowImgJson["img"] = nowImgList
        with open('./newimg/imgList.json', 'w', encoding='utf-8') as f:
            json.dump(nowImgJson, f)

        downloadImg(downLoadList)



def downloadImg(imgList):
    ipfs_gateWay = ['https://cf-ipfs.com/ipfs/', 'https://ipfs.io/ipfs/', 'https://dweb.link/ipfs/']
    for img_url in imgList:
        img_name = img_url
        if img_url == "":
            continue
        # download img
        if 'https' not in img_url:
            img_url = random.choice(ipfs_gateWay) + img_name

        response = requests.get(img_url)
        print('Download ' + img_url + '✔️')
        # write to file
        f_img = open('./newimg/' + img_name, 'wb')
        f_img.write(response.content)
        f_img.close()

if __name__ == '__main__':
    args = sys.argv[1:]
    try:
        opts, arg = getopt.getopt(args, "db", ["deploy",  "backupimg"])
    except getopt.GetoptError:
        print('参数解析错误')

    for opt, arg in opts:
        if opt in ('-d', '--deploy'):
            filelist = []
            postlist = []
            fontChar()
            # addDiscussion()
        elif opt in ('-b', '--backupimg'):
            filelist = []

            if os.path.exists('./imgList.json'):
                imgJson = []
                with open('./imgList.json', 'r', encoding='utf-8') as f:
                    imgJson = json.load(f)
            else:
                imgJson = {}
                imgJson["img"] = []

            backupImg()
        else:
            print('''参数错误''')
