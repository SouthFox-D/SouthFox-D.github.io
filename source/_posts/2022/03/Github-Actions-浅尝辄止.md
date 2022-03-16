---
author: SouthFox
title: Github Actions 浅尝辄止
date: 2022-03-07 16:32:14
tags:
- 技术
category: 技术
toc: true
---

免费送！免费送！微软电脑免费送啦！

<!-- more -->

自从 `GitHub` 被微软收购以后确实是有钱了啊，`GitHub Actions` 公开仓库竟然可以免费用（私有仓库也有三千分钟时间）。

什么？不知道 `GitHub Actions` 是什么？

简单来说就是提前写好对应的配置文件，那么 `GitHub` 就会在满足条件时分配电脑执行对应的配置文件，配置文件可以包含命令或者引用其他人写好的配置文件……所以这如同有一台真实的服务器一般强大。

本来呢，这种服务是为了方便一众开源应用测试和编译的，但是被一众羊毛党薅出花来了……什么自动签到啦、生成订阅源啦、推送消息啦等等等等，这些「奇技淫巧」可以在各家推荐专栏找到，有兴趣可以自己去找一下。

还是说说我拿 `GitHub Actions` 来干什么吧。

## Hexo 自动部署

如果经管着一个静态博客那么这功能是刚需了吧，毕竟如果想在移动设备或者不方便安装环境的公用电脑上写作的话，那么让 `Actions` 进行生成静态页面和部署是一个优秀的选择。



### 生成 Personal access tokens

因为分配的机器是一个干净的环境，并没有操作对应 `GitHub` 仓库的能力，为了做到登陆，首先需要生成一个 `access tokens` 。（当然也可以用私钥认证，但是感觉麻烦就没用这方式……）

首先到 [GitHub 配置页](https://github.com/settings/tokens) 点击 `Generate new token` 按钮，输入密码认证后跳转到对应页面。

`Note` 填写一个便于理解的名称，我这边是用来自动部署 `Hexo` 博客就写 `HEXO_DEPLOY ` 。

然后选择过期日期（`Expiration`），如果不嫌麻烦并确信自己不会忘记更换，那么可以设置一年两年的过期时限，否则懒人就选永不过期吧（ `No expiration`）吧……

然后在权限范围（`Select scopes`）栏里勾选 `repo` 框（如果能明确对应权限是干嘛的也可以只勾选需要的权限）。

然后点击生成按钮(`Generate token`)生成一个新的 `access tokens` ，这个页面只显示一次，所以不要关闭窗口！



### 设置 Actions secrets

除非你是私有仓库，那么运行 `actions` 的配置文件和所有输出对所有人都是可见的，为了安全，要自己设定一个`Actions secrets` 隐藏相关的 `token` 。

新开一个页面，到存储 `Hexo` 博客文件的仓库， `Settins -> Secrets -> Actions -> 点击 New repository secret 按钮` ，然后新页面 `Name` 填写便于理解的名称 ，我这边是用来自动部署 `Hexo` 博客就写 `HEXO_DEPLOY ` 。值（`Value`）里填写上一步 `Generate token` 生成的 `token` ，然后点击 `  Add secret` 保存 `secrets` 。



### 编写配置文件

做好前期工作后就到激动人心的写配置时间喽。

首先在 `Git` 仓库根目录下新建一个 `.github` 文件夹，然后再在里面新建一个 `workflows` 文件夹，再在里面新建一个 `xxx.yml` 文件，我的形式是 `仓库/.github/workflows/deploy.yml` 。

之后就该往 `.yml` 文件写配置了，`yml` 格式是一个严格缩进格式，所以对自己好点，用 `VS Code` 之类的编辑器来写……

```yaml
name: Hexo Deploy

on:
  push:
    branches: [ hexo ]

jobs:
  build:

    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      
```

 

`name` 那一栏填便于理解的名字。

`on` 选项是为监听哪个分支发生变动就执行动作，我这边设置为 `push` 的 `hexo` 分支，意为当 `hexo` 分支下发生了 `push` 动作后就执行此 `action` ，分支名请填自己仓库实际的，是 `main`还是 `hexo` 请根据实际选择。

`jobs` 下的名称大概是实际执行 `action` 时的名字，可写个便于理解的名字。

`runs-on` 是声明需要什么类型的机器， `ubuntu-latest` 就是申请最新的 `ubuntu` 系统的机子。

`fail-fast` 的实际用途我也不懂，反正他就在那儿了.jpg 。

  ```yaml

    steps: 
    - uses: actions/checkout@v2 #拉取仓库文件
    
    - name: Set up Node.js
      uses: actions/setup-node@v1
      with:
        node-version: 17
        
    - name: Install dependencies
      run: |
        npm install -g hexo-cli
        npm install
        git config --global user.name "wherever"
        git config --global user.email "wherever@xxx.xx"
    - name: Deploy
      env:
        DEPLOY_KEY: ${{ secrets.HEXO_DEPLOY }}
      run: |
        hexo clean
        hexo generate
        cd ./public
        git init
        git add --all .
        git commit -m "GitHub action Auto Builder"
        git push --quiet --force https://$DEPLOY_KEY@github.com/name/name.github.io.git master

  ```

之后就是 `steps` 栏目了，`name` 写个便于理解的名字。

`GitHub actions` 最为被称道的一点就是能用其他人预先写好的脚本，所以在 `Set up Node.js` 栏里的 `actions/setup-node@v1` 这里就是引用了安装 `nodejs`  环境的脚本，`with` 后面填入脚本里预先定义好的变量（填 17 意为安装版本号为 17 的  `node` ），不用自己考虑如何实现。

`Install dependencies` 这栏就是安装依赖了，`npm` 安装 `Hexo` 的包，然后 `git config` 是设置自己的名称和电子邮箱。

`Deploy` 的 `env` 一栏就是上面设置 `Actions secrets` 操作起到用处的地方了，

`DEPLOY_KEY: ${{ secrets.HEXO_DEPLOY }}` 

的意思就是把 `Actions secrets` 里的叫 `HEXO_DEPLOY` 的值声明成叫 `DEPLOY_KEY` 的环境变量，按自己实际情况写……

下面的命令就是重复流水帐了…… `git push --quiet --force https://$DEPLOY_KEY@github.com/name/name.github.io.git master` 这里就根据实际情况写了，我是博客源文件和生成的静态文件都放一个仓库里就将生成好的静态文件用 `--force` 参数强制部署到 `master` 分支，**要提前切换好分支！**在自己仓库的 `Settings -> Branches` 的 `Default branch` 里将自己放置博客源代码的分支改名并设为默认（我是将放博客源代码的命名为 `hexo`），请提前确认好，要不然 `--force` 参数将会连同历史记录**覆盖**掉 `master` 分支的内容！

如果是分出一个仓库放置生成后的静态页面就根据自己实际情况写。



### 测试

之后就是进行 `push` 操作看看 `action` 有没有正常工作了，要是失败就会出现红色的叉叉，这时请根据提示检查哪里出了问题（文件缩进、名称没写对等……）。

成功了的话，就去检查对应分支有没有出现对应的静态文件了。



## 部署自定义字体

当然……因为是一台对应的机器，所以可以装点 `Python` 环境运行一点脚本啊……我之前[中文字体动态剪裁](../../../2021/03/中文网页字体动态裁剪/)就是用 `GitHub actions` 执行的。

### 具体而言……

```yaml
    steps: #安装 Python 环境
    - uses: actions/checkout@v2
    - name: Set up Python 3.9
      uses: actions/setup-python@v2
      with:
        python-version: 3.9
        
    - name: Install dependencies
      run: |
        python -m pip install --upgrade pip
        python -m pip install fonttools brotli requests

    - name: Deploy
      run: |
        python3 utils.py -d #运行提前写好的脚本生成字符集
        pyftsubset Zpix.ttf --text-file=strdb.txt #裁剪
        fonttools ttLib.woff2 compress -o Zpix.woff2 Zpix.subset.ttf
        mv Zpix.woff2 ./public/css/fonts
        mv Zpix.subset.ttf ./public/css/fonts/Zpix.ttf #将裁剪好的文件放到生成文件夹内
```



## 初始化 Giscus 对应评论

### 脚本文件

[参考这](../../01/为博客支持评论系统/)。

### 配置文件

[参考这](https://github.com/SouthFox-D/SouthFox-D.github.io/blob/hexo/.github/workflows/deploy.yml)……



## 图片备份

虽然博客用了 [`IPFS`](/2022/03/IPFS和博客/) 来当博客图床，但是也有失效的风险，所以做好备份是肯定的啦~

### 脚本文件：

跟[本地版](../IPFS和博客/)的区别不大，就是将图片放到一个文件夹方便后续操作……

[如果真想看……](https://github.com/SouthFox-D/SouthFox-D.github.io/blob/hexo/utils.py)

### 配置文件

```yaml
    steps:
    - uses: actions/checkout@v2  # 安装 Python 环境
    - name: Set up Python 3.9
      uses: actions/setup-python@v2
      with:
        python-version: 3.9
        
    - name: Install dependencies
      run: |
        sudo apt-get install wget
        python -m pip install --upgrade pip
        python -m pip install fonttools brotli requests
        git config --global user.name "SouthFox-D" #自己的……
        git config --global user.email "southfoxdreamer@gmail.com"
    - name: Check Img
      run: |
        wget https://raw.githubusercontent.com/SouthFox-D/blog_img/main/imgList.json #改成自己的仓库并已存在此文件
        python3 utils.py -b
    - name: Backup
      env:
        DEPLOY_KEY: ${{ secrets.HEXO_DEPLOY }} #自己的……
      run: |
        rm -rf .git #删除 Hexo 的 .git 文件夹防止后续冲突
        git clone https://github.com/SouthFox-D/blog_img.git blog_img #备份仓库
        cp -frp newimg/* blog_img/ #合并文件夹
        cd blog_img
        git add .
        git diff --exit-code || git commit -am "Auto backup" #防止报错
        git push --force https://$DEPLOY_KEY@github.com/SouthFox-D/blog_img.git #根据实际情况而定
```

具体可[参考这](https://github.com/SouthFox-D/SouthFox-D.github.io/blob/hexo/.github/workflows/backup-img.yml)……



## 总之

一次编写后，后续就不用怎么管了，自动化的好处体现出来了……

我在一台没有安装 `Hexo` 环境下的机器上写作，到时只要直接 `push` 就让 `GitHub` 来完成对应的部署操作了，不必自己安装环境又 `clear ` 啊 `g` 啊 `d` 啊的……在移动设备写作可以写好直接复制到 `GitHub` 网页版里，同时图片也不必当心失效。

反正……微软！有钱的！嫖起来没负罪感。 
