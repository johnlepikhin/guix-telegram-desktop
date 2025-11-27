# Telegram Desktop Guix Channel

A Guix channel providing latest Telegram Desktop package with updated dependencies and enhanced functionality.

## Usage

Add this channel to your `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'telegram-desktop)
        (branch "main")
        ;; or
        (commit "v6.3.4")
        (url "https://github.com/johnlepikhin/guix-telegram-desktop"))
       %default-channels)
```

Then install with: `guix install telegram-desktop`
