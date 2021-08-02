# Slack archive viewer

Original code from
[jekyll-slack](https://github.com/mdlincoln/jekyll-slack.git), with
modifications by [@benknoble](https://github.com/benknoble).

## Usage

### Deploy with GitHub actions

- Fork this repository
- Add your archives in `archives/`
  - Make sure the names sort in chronological order. A simple way to do so is to
    use `from-YYYY-MM-DD-to-YYYY-MM-DD.zip` as the name.
- Copy `pages.yml` to `.github/workflows/pages.yml`

The site will be built on pushes to `main`.

### Deploy manually

You'll need racket 8.0+ and Ruby 3.0+ with bundler.

- Clone this repository
- Add your archives in a directory (e.g., `archives`)
  - Make sure the names sort in chronological order. A simple way to do so is to
    use `from-YYYY-MM-DD-to-YYYY-MM-DD.zip` as the name.
- Run `racket unpack.rkt <your-archive-dir>`
  - Installing the racket package with `raco pkg install` or `make install` will
    cause the racket code to run faster. Then the unpack step can be run as
    `racket -l slack-archive-viewer/unpack <your-archive-dir>`.
- Run `bundle && bundle exec jekyll build`

The site will be built into `_site`.

## Developing

WIP: port to Racket

Todo:

- [x] Support file comment messages
- [x] Pages deploy
- [x] Merge subsequent archives chronologically
- [ ] Threads
- [ ] Fix triple-backtick markdown
- [ ] HTML decode
- [ ] Support attachments (related to bot messages)
- [ ] Support messages from bots (there are several "kinds")
