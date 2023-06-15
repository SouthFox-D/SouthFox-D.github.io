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
    mastodonComment =
      `<div class="mastodon-comment" style="margin-left: calc(10px * ${depth})">
        <div class="author">
          <div class="avatar">
            <img src="${escapeHtml(toot.account.avatar_static)}" height=60 width=60 alt="">
          </div>
          <div class="details">
            <a class="name" href="${toot.account.url}" rel="nofollow">${toot.account.display_name}</a>
            <a class="user" href="${toot.account.url}" rel="nofollow">${user_account(toot.account)}</a>
          </div>
          <a class="date" href="${toot.url}" rel="nofollow">${toot.created_at.substr(0, 10)} ${toot.created_at.substr(11, 8)}</a>
        </div>
        <div class="content">${toot.content}</div>
        <div class="status">
          <div class="replies ${toot_active(toot, 'replies')}">
            <a href="${toot.url}" rel="nofollow"><i class="fa fa-reply fa-fw"></i>${toot_count(toot, 'replies')}</a>
          </div>
          <div class="reblogs ${toot_active(toot, 'reblogs')}">
            <a href="${toot.url}" rel="nofollow"><i class="fa fa-retweet fa-fw"></i>${toot_count(toot, 'reblogs')}</a>
          </div>
          <div class="favourites ${toot_active(toot, 'favourites')}">
            <a href="${toot.url}" rel="nofollow"><i class="fa fa-star fa-fw"></i>${toot_count(toot, 'favourites')}</a>
          </div>
        </div>
      </div>`;
    document.getElementById('mastodon-comments-list').innerHTML += mastodonComment

    render_toots(toots, toot.id, depth + 1)
  }

  function loadComments() {
    if (commentsLoaded) return;

    document.getElementById("mastodon-comments-list").innerHTML = "Loading comments from the Fediverse...";
    const mastodonApiUrl = post_url.replace(/@[^\/]+/, 'api/v1/statuses') + '/context';

    fetch(mastodonApiUrl)
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        if(data['descendants'] && Array.isArray(data['descendants']) && data['descendants'].length > 0) {
            document.getElementById('mastodon-comments-list').innerHTML = "";
            render_toots(data['descendants'], id, 0)
        } else {
          document.getElementById('mastodon-comments-list').innerHTML = "<p>Not comments found</p>";
        }

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

