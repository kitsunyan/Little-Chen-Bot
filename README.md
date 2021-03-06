# Little Chen Bot

Telegram bot that allows you to fetch images using iqdb and google.

## Dependencies

`libwebp` — Allows to convert WebP images to PNG.  
`imagemagick` — Allows to transform and create images.  
`fonts-roboto` — Font for images.  
`nodejs` — Tokenless Google Translate.  
`ffmpeg` — For audio converting.

## Building and Running

Run `sbt run` to run bot and `sbt proguard:proguard` to build a standalone jar file.

### Build Configuration

Build configuration is stored in `build.conf`.

Available properties:

* `solib.os` — List of supported OS (list of String values)
* `solib.arch` — List of supported architectures (list of String values)

With both `solib.os` and `solid.arch` properties provided, the final jar file will contain only `*.so` libraries for specified OS and architectures.

Example configuration:

```properties
solib {
    os = [
        "Linux"
    ]
    arch = [
        "x86_64",
        "armv7"
    ]
}
```

## Configuration

You should create `littlechenbot.conf` in the project directory.

Available properties:

* `bot.token` — Telegram API token (`String`, required)
* `bot.owner` — Bot owner ID to log errors (`Long`)
* `bot.workspace` — Chat ID for bot's internal storage (`Long`)
* `bot.chats` — List of allowed chats (list of `(id: Long, alias: String)`)
* `bot.chatsAnyPrivate` — Allows to reply to all private chats (`Boolean`)
* `bot.chatsAnyGroup` — Allows to reply to all group chats (`Boolean`)
* `bot.proxy.host` — Proxy host (`String`)
* `bot.proxy.port` — Proxy port (`Int`)
* `bot.proxy.type` — Proxy type: http, socks, direct (`String`)
* `bot.proxy.restart` — Command to restart proxy (list of `String`)
* `bot.proxy.whitelist` — Proxy will not be used for these hosts (list of `String`)
* `bot.binaries.dwebp` — Path to WebP decompresser binary (`String`)
* `bot.binaries.ffmpeg` — Path to FFmpeg binary (`String`)
* `bot.binaries.magick` — Path to ImageMagick binary (`String`)
* `bot.binaries.node` — Path to Node.js binary (`String`)

A `bot.workspace` parameter allows to save some data using chat messages. A special chat should be created for a bot where it will send messages and obtain them later. It's possible in Telegtam to create a group and leave it then leaving only a bot in the chat.

Example configuration:

```properties
bot {
    token = "385946732:Jsi19If0si1_dj8AojOS9N5dhsJc8jJ7S98"
    chatsAnyPrivate = true
    chats = [
        {
            id = 83420754235,
            alias = "chat1"
        },
        {
            id = -4759243795,
            alias = "chat2"
        }
    ]
    proxy = {
        host = "localhost"
        port = 9050
        type = "socks"
        restart = [ "/usr/bin/systemctl" "restart" "tor" ]
    }
}
```
