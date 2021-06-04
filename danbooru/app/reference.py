
# https://www.bilibili.com/read/cv11565208

import requests
from bs4 import BeautifulSoup
import re


def getData(url):
    try:
        head = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) \
            AppleWebKit/537.36 (KHTML, like Gecko) \
            Chrome/88.0.4324.146 Safari/537.36'
        }
        r = requests.get(url, headers=head, timeout=30)
        r.raise_for_status()
        r.encoding = r.apparent_encoding
        return r.text
    except Exception:
        print('网页请求出现错误')
        return ''


def getEveryJpgUrl(webData, jpgUrlList):
    try:
        soup = BeautifulSoup(webData, 'html.parser')
        a = soup.find_all('a', {'href': re.compile(r'^/posts/[0-9]+.q=dise')})
        for href in a:
            jpgUrlList.append('https://danbooru.donmai.us' + href['href'])
        # print(jpgUrlList)
    except Exception:
        print('获取单个图片地址时出现问题')
        pass


def getEveryJpg(webData, jpgList):
    try:
        soup = BeautifulSoup(webData, 'html.parser')
        li = soup.find('li', id='post-info-size')
        # print(li)
        a = li.find('a')
        jpgList.append(a['href'])
        # print(jpgList)
    except Exception:
        print('获取图片时出现问题')
        pass


def downloadJpg(jpgList):
    global count
    for jpg in jpgList:
        print("正在保存第{}张".format(count))
        try:
            head = {
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) \
                        AppleWebKit/537.36 (KHTML, like Gecko) \
                        Chrome/88.0.4324.146 Safari/537.36'
            }
            jpgData = requests.get(jpg, timeout=30, headers=head)
            jpgData.raise_for_status()

            with open(str(count) + '.jpg', 'wb+') as f:
                f.write(jpgData.content)
            count += 1
        except Exception:
            print("第{}张保存失败".format(count))
            count += 1
            continue


if __name__ == '__main__':
    count = 1
    for i in range(1, 15):
        print("正在下载第{}页".format(i))
        url = 'https://danbooru.donmai.us/posts?page=' + str(i) + '&tags=dise'
        jpgUrlList = []
        jpgList = []

        data = getData(url)
        getEveryJpgUrl(data, jpgUrlList)

        for jpgUrl in jpgUrlList:
            jpgData = getData(jpgUrl)
            getEveryJpg(jpgData, jpgList)

        downloadJpg(jpgList)
