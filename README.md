# scala-practice1

### Development

```bash
docker run --rm -it -v $(pwd):/root sbtscala/scala-sbt:eclipse-temurin-alpine-21.0.2_13_1.10.1_3.5.0 bash
```

#### create new project

```bash
sbt new scala/scala3.g8
```

#### sbt

start sbt

```bash
~# sbt
[info] welcome to sbt 1.10.2 (Eclipse Adoptium Java 21.0.2)
[info] loading project definition from /root/project
[info] loading settings for project root from build.sbt ...
[info] set current project to scala-practice1 (in build file:/root/)
[info] sbt server started at local:///root/.sbt/1.0/server/27dc1aa3fdf4049b492d/sock
[info] started sbt server
sbt:scala-practice1>
```

run

```bash
sbt:scala-practice1> run
[info] running hello
Hello world!
I was compiled by Scala 3. :)
[success] Total time: 2 s, completed Oct 4, 2024, 1:53:04 PM
```

test

```bash
sbt:scala-practice1> test
MySuite:
  + example test that succeeds 0.016s
[info] Passed: Total 1, Failed 0, Errors 0, Passed 1
[success] Total time: 1 s, completed Oct 4, 2024, 1:53:07 PM
sbt:scala-practice1>
```
