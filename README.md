# Slack archive viewer

A racket program based on [jekyll-slack](https://github.com/mdlincoln/jekyll-slack.git).

## Usage

### Deploy with GitHub actions

- Create a (probably private) repository.
- Add your archives in `archives/`.
  - Make sure the names sort in chronological order. A simple way to do so is to
    use `from-YYYY-MM-DD-to-YYYY-MM-DD.zip` as the name.
- Copy `pages.yml` to `.github/workflows/pages.yml` and modify the `folder` to
  push to `gh-pages`.

The site will be built on pushes to `main`.

### Deploy manually

You'll need racket 8.0+.

- `raco pkg install slack-archive-viewer`
- Add your archives in a directory (e.g., `archives`).
  - Make sure the names sort in chronological order. A simple way to do so is to
    use `from-YYYY-MM-DD-to-YYYY-MM-DD.zip` as the name.
- Configure the build metadata and privacy lists (see `slack-config-demo.rkt`
  and `privacy-list-demo.rkt`).
- Run `racket -l slack-archive-viewer <your-archive-dir>`

The site will be built into `_site` unless you specify `base-url` in
`slack-config.rkt`, in which case the site is built into a matching directory
(sans leading slash).

To view the generated site:
- If your site was built into `_site` (no `base-url`), you can run a web server
  in the `_site` directory (*e.g.*, `python3 -m http.server -d _site`) and
  navigate to `localhost:XXXX` in a browser.
- If your site was built into a `base-url` directory, you can run a web server
  from the build directory (*e.g.*, `python3 -m http.server`) and navigate to
  `localhost:XXXX/base-url` in a browser.

## Developing

Todo:

- [x] Support file comment messages
- [x] Pages deploy
- [x] Merge subsequent archives chronologically
- [ ] Threads
- [ ] Fix triple-backtick markdown (Slack might be getting rid of newlines around triple-backticks)
- [ ] HTML decode (Slack HTML encodes < and >, even in code blocks, in the text of messages. This is wrong, but too bad for us?)
- [ ] Support attachments (related to bot messages)
- [ ] Support messages from bots (there are several "kinds")
