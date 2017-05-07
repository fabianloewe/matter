# The Matter Language

Matter is a _compiler creation language \(a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)\)_ for easily building parsers, [ASTs](https://en.wikipedia.org/wiki/Abstract_syntax_tree), interpreters and, of course, compilers in one simple language.

Wether you want to work on an existing programming language or create your own one, Matter helps you and saves you time.

## Features
#### Language

* Static typed and [type-inferred](https://en.wikipedia.org/wiki/Type_inference) language
* Simple syntax inspired by Ruby and Python
* Clear seperation of each part \(_parsing_, _AST generation_ and _compilation_\) while still keeping all together in its type \(see the [Documentation](https://www.gitbook.com/book/hyronx/matter-lang/details)\)
* Extendable compilation support for machine code, the JVM, etc. \(through the [Compiler](https://www.gitbook.com/book/hyronx/matter-lang/details) interface\)

#### Management

* Shareable and managed language definition packages
* Shareable and managed compiler backend packages
* Browseable package registry
* Automatically Git versioned

## Installation

Download and install [sbt](http://www.scala-sbt.org/download.html).
Then clone this repository:
```bash
git clone https://github.com/hyronx/matter
cd matter
sbt stage
```

Now you should be able to run the Matter cli by entering:

Linux / OS X:
```bash
./target/universal/stage/bin/matter-compiler
```

Windows:
```batch
.\target\universal\stage\bin\matter-compiler.bat
```

If you got any problems, please have a look at the [issue](README.md#Issue) section.

## Usage

To create a new project:
```bash
matter-compiler new <project-name>
```

This will create a new directory in your current working directory called `<project-name>`. It should have the following structure:
```bash
<project-name>
|-- build
|-- src
|   |-- main
|   |   `--matter
|   |      `--<project-name>
|   `-- test/
`-- config.yaml
```

Now you can place your source files in `src/main/matter/<project-name>` and compile them by entering:
```bash
matter-compiler compile
```
which should result in some Java class files in the `build` directory.

Executing your compiler/parser/etc. is currently quite complicated:
```bash
# Move to the directory where Matter is located
cd..  
# Run java
java -cp matter/target/universal/stage/lib/com.lihaoyi.fastparse_2.12-0.4.2.jar:\ # the internal parser library
matter/target/universal/stage/lib/com.lihaoyi.fastparse-utils_2.12-0.4.2.jar:\ # the internal parser library
matter/target/universal/stage/lib/com.lihaoyi.sourcecode_2.12-0.1.3.jar:\ # the internal parser library
matter/target/universal/stage/lib/de.hyronx.matter-compiler-0.0.1.jar:\ # some support classes
matter/target/universal/stage/lib/org.scala-lang.scala-library-2.12.1.jar:\ # Scala language runtime
<project-name>/build/ \
<project-name>.Compiler \
<file-to-parse>
```

I will improve this as soon as all basic features are implemented.

Again if you got any problems, please have a look at the next section.

## Issue

If something didn't work, you have two options:

1. Ask for help on [Gitter](https://gitter.im/matter-lang/Lobby). I will be happy to assist you with any problems.
2. Check if this problem is already reported on [GitHub](https://github.com/hyronx/matter/issues) and if not, open a new issue.

## Roadmap

* [x] Implement the language parser
* [x] Generate parsers based on Matter source code
* [x] Generate mappings from syntax to usable data
* [ ] Define the Compiler interface
* [ ] Implement the Compiler interface for the JVM

For more information see ROADMAP.md

## Contribution

See CONTRIBUTION.md

## License

Apache License V2.0

For more information see LICENSE

## Authors

First name or username followed by e-mail address in parenthesis, e.g.:
hyronx (hyronx@outlook.com)

See AUTHORS
