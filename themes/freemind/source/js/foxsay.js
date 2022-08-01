let parser = new RSSParser();
parser.parseURL('https://foxsay.southfox.me/@SouthFox.rss', function(err, feed) {
  if (err) throw err;
  for (let i = 0; i < 10; i++) {
    const entry = feed.items[i];
    printPost(entry);
  }
});

function printPost(entry) {
  section = document.createElement("section");
  section.classList.add('item');
  popContent = entry.contentSnippet;

  section.innerHTML = popContent;

  blogs = document.getElementById('foxsay');
  blogs.append(section);
}
