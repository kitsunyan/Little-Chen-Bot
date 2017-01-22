# Little Chen Bot

Telegram bot which allows you to fetch images from iqdb.

## Building and Installing

Run `sbt run` to run bot and `sbt assembly` to build a standalone jar file.

## Configuration

You should create `littlechenbot.conf` in the project directory.

You should specify `bot.token` key with Telegram API token.

You should also specify a list allowed chat IDs using `bot.chats` key. Bot will reply only to these chats.

Example configuration:

```properties
bot {
	token = "385946732:Jsi19If0si1_dj8AojOS9N5dhsJc8jJ7S98"
	chats = [
		83420754235, // Chat 1
		-4759243795 // Chat 2
	]
}
```

## Dependencies

`libwebp` — Allows to convert WebP images to PNG.  
`tor` — Bot uses Tor to access *booru servers.
