var post_url = document.currentScript.getAttribute("post-url");
var id = post_url.split('/').pop()


  function escapeHtml(unsafe) {
    return unsafe
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }

  var commentsLoaded = false;

  function toot_active(toot, what) {
    var count = toot[what+'_count'];
    return count > 0 ? 'active' : '';
  }

  function toot_count(toot, what) {
    var count = toot[what+'_count'];
    return count > 0 ? count : '';
  }

  function user_account(account) {
    var result =`@${account.acct}`;
    if (account.acct.indexOf('@') === -1) {
      var domain = new URL(account.url)
      result += `@${domain.hostname}`
    }
    return result;
  }

  function render_toots(toots, in_reply_to, depth) {
    var tootsToRender = toots.filter(toot => toot.in_reply_to_id === in_reply_to);
    tootsToRender.forEach(toot => render_toot(toots, toot, depth));
  }

  function render_toot(toots, toot, depth) {
    toot.account.display_name = escapeHtml(toot.account.display_name);
    toot.account.emojis.forEach(emoji => {
      toot.account.display_name = toot.account.display_name.replace(`:${emoji.shortcode}:`, `<img src="${escapeHtml(emoji.static_url)}" alt="Emoji ${emoji.shortcode}" height="20" width="20" />`);
    });
    toot.emojis.forEach(emoji => {
      toot.content = toot.content.replace(`:${emoji.shortcode}:`, `<img src="${escapeHtml(emoji.url)}" alt="Emoji ${emoji.shortcode}" height="20" width="20" />`);
    });
    mastodonComment =
      `<div class="mastodon-comment" style="margin-left: calc(10px * ${depth})">
        <div class="author">
          <div class="avatar">
            <img src="${escapeHtml(toot.account.avatar_static)}" height=60 width=60 alt="">
          </div>
          <div class="details">
            <a target="_blank" class="name" href="${toot.account.url}" rel="nofollow">${toot.account.display_name}</a>
            <a target="_blank" class="user" href="${toot.account.url}" rel="nofollow">${user_account(toot.account)}</a>
          </div>
          <a target="_blank" class="date" href="${toot.url}" rel="nofollow">${toot.created_at.substr(0, 10)} ${toot.created_at.substr(11, 8)}</a>
        </div>
        <div class="content">${toot.content}</div>
        <div class="status">
          <div class="replies ${toot_active(toot, 'replies')}">
            <a target="_blank" href="${toot.url}" rel="nofollow"><i class="fa fa-reply fa-fw"></i>${toot_count(toot, 'replies')}</a>
          </div>
          <div class="reblogs ${toot_active(toot, 'reblogs')}">
            <a target="_blank" href="${toot.url}" rel="nofollow"><i class="fa fa-retweet fa-fw"></i>${toot_count(toot, 'reblogs')}</a>
          </div>
          <div class="favourites ${toot_active(toot, 'favourites')}">
            <a target="_blank" href="${toot.url}" rel="nofollow"><i class="fa fa-star fa-fw"></i>${toot_count(toot, 'favourites')}</a>
          </div>
        </div>
      </div>`;

    document.getElementById('mastodon-comments-list').innerHTML += mastodonComment

    render_toots(toots, toot.id, depth + 1)
  }

  function loadComments() {
    if (commentsLoaded) return;

    document.getElementById("mastodon-comments-list").innerHTML = "加载评论中……";
    const mastodonApiUrl = post_url.replace(/@[^\/]+/, 'api/v1/statuses') + '/context';

    fetch(mastodonApiUrl)
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        document.getElementById('mastodon-comments-list').innerHTML = `<p><button class="addComment">进行评论</button></p>`
        if(data['descendants'] && Array.isArray(data['descendants']) && data['descendants'].length > 0) {
            render_toots(data['descendants'], id, 0)
        } else {
          document.getElementById('mastodon-comments-list').innerHTML += "<p>此文章下没有公开的评论。</p>";
        }
        document.getElementById('mastodon-comments-list').innerHTML += `
          <dialog id="comment-dialog">
              <h3>进行评论</h3>
              <button title="Cancel" id="close">&times;</button>
              <p>
                  评论由一个 Mastodon 网站提供，所以可通过一个联邦宇宙帐号回复这个帖子进行评论。在下面填入自己的网站地址来跳转到自己网站中进行互动：
              <p>
              <p class="input-row">
                  <input type="text" inputmode="url" autocapitalize="none" autocomplete="off"
                      value="${ localStorage.getItem("post_url") ?? '' }" id="instanceName"
                      placeholder="例如：mastodon.coffee">
                  <button class="button" id="go">前往</button>
              </p>
              <p>或是复制帖文的地址并通过自己网站的搜索框抓取这篇帖子进行互动：</p>
              <p class="input-row">
                  <input type="text" readonly id="copyInput" value="${ post_url }">
                  <button class="button" id="copy">复制</button>
              </p>
          </dialog>`
        const dialog = document.getElementById('comment-dialog');

                // open dialog on button click
                Array.from(document.getElementsByClassName("addComment")).forEach(button => button.addEventListener('click', () => {
                    dialog.showModal();
                    // this is a very very crude way of not focusing the field on a mobile device.
                    // the reason we don't want to do this, is because that will push the modal out of view
                    if(dialog.getBoundingClientRect().y > 100) {
                        document.getElementById("instanceName").focus();
                    }
                }));

                // when click on 'Go' button: go to the instance specified by the user
                document.getElementById('go').addEventListener('click', () => {
                    let url = document.getElementById('instanceName').value.trim();
                  if (url === '') {
      // bail out - window.alert is not very elegant, but it works
      window.alert("请填入你的网站地址！");
      return;
        }

        // store the url in the local storage for next time
        localStorage.setItem('mastodonUrl', url);

        if (!url.startsWith('https://')) {
      url = `https://${url}`;
        }

        window.open(`${url}/authorize_interaction?uri=${post_url}`, '_blank');
                });

                // also when pressing enter in the input field
                document.getElementById('instanceName').addEventListener('keydown', e => {
                    if (e.key === 'Enter') {
                        document.getElementById('go').dispatchEvent(new Event('click'));
                    }
                });

                // copy tye post's url when pressing copy
                document.getElementById('copy').addEventListener('click', () => {
                    // select the input field, both for visual feedback, and so that the user can use CTRL/CMD+C for manual copying, if they don't trust you
                    document.getElementById('copyInput').select();
                    navigator.clipboard.writeText(post_url);
                    // Confirm this by changing the button text
                    document.getElementById('copy').innerHTML = '已复制！';
                    // restore button text after a second.
                    window.setTimeout(() => {
                        document.getElementById('copy').innerHTML = '复制';
                    }, 1000);
                });

                // close dialog on button click, or escape button
                document.getElementById('close').addEventListener('click', () => {
                    dialog.close();
                });
                dialog.addEventListener('keydown', e => {
                    if (e.key === 'Escape') dialog.close();
                });

                // Close dialog, if clicked on backdrop
                dialog.addEventListener('click', event => {
                    var rect = dialog.getBoundingClientRect();
                    var isInDialog=
                           rect.top <= event.clientY
                        && event.clientY <=rect.top + rect.height
                        && rect.left<=event.clientX
                        && event.clientX <=rect.left + rect.width;
                    if (!isInDialog) {
                        dialog.close();
                    }
                })

        commentsLoaded = true;
      });
  }

  function respondToVisibility(element, callback) {
    var options = {
      root: null,
    };

    var observer = new IntersectionObserver((entries, observer) => {
      entries.forEach(entry => {
        if (entry.intersectionRatio > 0) {
          callback();
        }
      });
    }, options);

    observer.observe(element);
  }

var comments = document.getElementById("mastodon-comments-list");
respondToVisibility(comments, loadComments);

