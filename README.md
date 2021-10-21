twutter
---

Toy project, which is a twitter clone built by using

- Scala 3
- FaunaDB
- ZIO
- Caliban

### Features

- Used Scala 3's new type class derivation for `FaunaEncoder` and `FaunaDecoder`
- Turns Java futures into `zio.Task` for seamless composition of effectful programs

### Future ideas

- Explore `ZQuery` or other concepts to resolve relational queries
- Streaming by turning a Java flow into a `ZStream`

### Reference

Crash course FaunaDB: https://www.youtube.com/watch?v=2CipVwISumA. Or follow the official guide: https://docs.fauna.com/fauna/current/tutorials/social_graph?lang=shell