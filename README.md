# Telegram Desktop Guix Channel

A Guix channel providing Telegram Desktop 6.1.4 package with updated dependencies and enhanced functionality.

This channel builds upon the official GNU Guix telegram-desktop package, updating it to version 6.1.4 and including
additional input method support (fcitx5-qt, nimf, hime) and image format libraries.

## Usage

Add this channel to your `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'telegram-desktop)
        (branch "main")
        ;; or
        (commit "v6.1.4")
        (url "https://github.com/johnlepikhin/guix-telegram-desktop"))
       %default-channels)
```

Then install with: `guix install telegram-desktop`
