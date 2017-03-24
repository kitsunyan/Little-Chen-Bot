# Little Chen Bot

Telegram bot that allows you to fetch images from iqdb.

## Dependencies

`libwebp` — Allows to convert WebP images to PNG.  
`tor` — Bot uses Tor to access *booru servers.

## Building and Running

Run `sbt run` to run bot and `sbt assembly` to build a standalone jar file.

## Configuration

You should create `littlechenbot.conf` in the project directory.

Available properties:

* `bot.token` — Telegram API token (String, required)
* `bot.owner` — Bot owner ID to log errors (Long, optional)
* `bot.chats` — List of allowed chat IDs (list of Long values, optional)
* `bot.chatsAnyPrivate` — Allows to reply to all private chats (Boolean, optional)
* `bot.chatsAnyGroup` — Allows to reply to all group chats (Boolean, optional)

Example configuration:

```properties
bot {
    token = "385946732:Jsi19If0si1_dj8AojOS9N5dhsJc8jJ7S98"
    chatsAnyPrivate = true
    chats = [
        83420754235, // Chat 1
        -4759243795 // Chat 2
    ]
}
```
