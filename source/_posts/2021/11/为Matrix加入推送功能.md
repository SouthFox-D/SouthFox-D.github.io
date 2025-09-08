author: SouthFox
title: 为 Matrix 加入自定义推送功能
date: 2021-11-22 15:53:18
tags: 技术, Matrix
category: 技术
toc: true
---

`Matrix` 是一个开源的聊天协议，和 `XMPP` 一样的联邦制设计也保证了像电子邮件一样的通信便捷。

<!-- more -->

好的地方很多，但最明显坏处就是推送了，对于即时通信来说，没有推送是非常大的不便……虽然搭建 `Matrix` 的应用 `Synapse` 配置文件启用邮箱的话可以启用邮件通知，不过鉴于邮件时效性也不是很高，所以需要个其他的推送手段。（谷歌市场下的 `Element` 附带了谷歌的消息推送，不过很遗憾在国内处于残废状态。）

所幸 `Synapse` 自带的 [API](https://spec.matrix.org/v1.1/client-server-api/) 可以定义推送规则，定义后帐号收到消息，服务器就可以将消息以 `JSON` 的形式推送到自定义的地址，来个曲线救国。



# Gotify

推送用到的工具为 `Gotify` ，特点是使用 `GO` 编写，高效快速，不过缺点就是安卓得常驻后台接收推送，不过至少要比腾讯全家桶吃的电要少。



## 下载

去 [Releases ](https://github.com/gotify/server/releases)处找到对应版本的下载链接，解压到`/opt/gotify/`。

```bash
unzip gotify-file -d /opt/gotify/
```

然后下载配置样板。

```bash
mkdir /etc/gotify
wget -O /etc/gotify/config.yml https://raw.githubusercontent.com/gotify/server/master/config.example.yml
```

编辑 `/etc/gotify/config.yml`。将 `port` 改为没有占用的端口， `name` 和 `pass` 是管理员的用户名和密码。



# Nginx 反代

在 `nginx` 的新建一个虚拟主机并在配置文件增加以下内容：

```nginx
location / {
  proxy_pass         http://localhost:8080;
  rewrite ^/gotify(/.*) $1 break;
  proxy_http_version 1.1;

  # Ensuring it can use websockets
  proxy_set_header   Upgrade $http_upgrade;
  proxy_set_header   Connection "upgrade";
  proxy_set_header   X-Real-IP $remote_addr;
  proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
  proxy_set_header   X-Forwarded-Proto http;
  proxy_redirect     http:// $scheme://;
}
```

> proxy_set_header   Connection "upgrade"; 这一栏不要漏了……要不然开启 WS 时会报错……
>
> 



# 设置服务

新建并编辑 `/etc/systemd/system/gotify.service`。

```
[Unit]
Description=gotify service
After=network.target
Wants=network.target

[Service]
Type=simple
PIDFile=/run/gotify.pid
WorkingDirectory=/opt/gotify
ExecStart=/opt/gotify/gotify-file
RestartPreventExitStatus=23
Restart=always
RestartSec=10s

[Install]
WantedBy=multi-user.target
```

然后开启服务并设置成开机启动。

```
systemctl start gotify
systemctl enable gotify
```

没有报错的话就可以打开 `Nginx` 里所设置的域名进入管理页面了，（改密码）之后就可以新建应用了，拿到推送令牌，就可以向服务器设置推送规则了。



## 设置推送

在官方文档里，通过 /_matrix/client/v3/pushers/set `API` 就可以设置推送规则了，通过 `curl` ，或者其他方式向服务器对应地址发送 `POST` 请求即可使用。

```bash
curl 'https://server_url/_matrix/client/r0/pushers/set' -H 'Authorization: Bearer access_token' -H 'Content-Type: application/json' -X POST -d '{"lang": "en","kind": "http","app_display_name": "Gotify","device_display_name": "Gotify","pushkey": "Gotify-PushKey","app_id": "zh.xxx.gotify","data": {"url": "https://Push_url/_matrix/push/v1/notify","format": "full_event"}}'
```

详细说明请查阅官方 [文档](https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3pushersset) , 其中 `access_token` 可以在应用里拿到，`pushkey ` 设置成 `Gotify-PushKey` 的样子是为了后续处理方便……现阶段推送地址必须包含 `/_matrix/push/v1/notify ` 路径，否则会报错，所以不得不进行额外处理了……



## 接收推送

服务器设置推送规则之后就会向对应地址发送数据了，因为推送地址现阶段必须包含 `/_matrix/push/v1/notify ` 路径，所以不得不再设置一个接收服务然后进行处理了。

接下来使用 `python` 的 `flask` 框架简单搭一个服务，首先先安装环境。

```python
pip3 install flask
pip3 install flask-apscheduler
```

然后是代码：

```python
import json
import requests
import datetime

from flask import Flask, jsonify, request
from flask_apscheduler import APScheduler
app = Flask(__name__)
scheduler = APScheduler()

scheduler.start()

def push_notification(push_data):
    if push_data["push_way"] == 'Gotify':
        resp = requests.post(f'https://push.xxx.xxx/message?token={push_data["push_token"]}', json={
        "message": f'「{push_data["sender"]}」发送了消息给你。',
        "priority": 8,
        "title": "新消息！"
        })
        print(resp)
        return
    if push_data["push_way"] == 'Bary':
        try:
            response = requests.post(
                url="https://api.day.app/push",
                headers={
                    "Content-Type": "application/json; charset=utf-8",
                },
                data=json.dumps({
                    "body": f'「{push_data["sender"]}」发送了消息给你。',
                    "device_key": push_data["push_token"],
                    "title": "新消息！",
                    "category": "category",
                    "sound": "minuet.caf",
                })
            )
            print('Response HTTP Status Code: {status_code}'.format(
                status_code=response.status_code))
            print('Response HTTP Response Body: {content}'.format(
                content=response.content))
        except requests.exceptions.RequestException:
            print('HTTP Request failed')
        return


# Our route that will receive the webhooks from Duffel's servers
@app.route('/_matrix/push/v1/notify', methods=['POST'])
def hello_world():
    push_data = {}
    event = None

    try:
        event = request.json
        print(event)

    except:
        return jsonify(success=False)

    # Handle the event
    if event["notification"]["type"] == 'm.room.message' or event["notification"]["type"] == 'm.room.encrypted':
        app_id = event["notification"]["devices"][0]["app_id"]
        push_data["push_way"], push_data["push_token"] = event["notification"]["devices"][0]["pushkey"].split('-')
        push_data["sender"] = event["notification"]["sender_display_name"]

        scheduler.add_job(
            func=push_notification,
            args=(push_data,),
            # trigger="date",
            next_run_time=datetime.datetime.now() + datetime.timedelta(seconds=25),
            id=app_id,
            replace_existing=True,
        )

        print('✅add push job!')

    if event["notification"]["type"] == None and event["notification"]["id"] == '':
        app_id = event["notification"]["devices"][0]["app_id"]
        try:
            scheduler.remove_job(id=app_id)
        except:
            pass
        print('✅remove push!')

    return jsonify(success=True)
```

然后用命令 `FLASK_ENV=development FLASK_RUN_PORT=4567 FLASK_APP=webhook.py flask run` 就可启用。再然后再用 `Nginx` 反代一下 `4567` 端口就行了，因为 `Synapse` 默认禁止本机访问，为了规范一点就另外开个站点接收请求吧……

基本逻辑就是接收 `JSON` 拿到发送者名称和推送令牌，判断是哪项服务，然后等待三十秒推送到对应的应用令牌上去，期间要是判断用户已经阅读了消息，就移除推送作业。

到此无意外的话，应该就能正常推送了，麻烦……希望以后能够有更友好便捷的方式吧。



# 参考

1. [在树莓派上部署消息推送软件Gotify](https://blog.mjyai.com/2021/02/24/raspberry-pi-gotify/)
