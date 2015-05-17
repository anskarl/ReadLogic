## ReadLogic

*ReadLogic* is a small open-source library for parsing logical expressions (ISO Prolog syntax) and is implemented in [Scala programming language](http://scala-lang.org). 

## Licence 

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0). Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

## Compilation instructions

In order to build *ReadLogic* from source, you need to have Java (version 7 or greater) and [sbt](http://www.scala-sbt.org) installed in your system.

To compile the project, type the following command:

```
$ sbt compile
```

To publish *ReadLogic* to your local Apache Ivy directory (e.g., inside ~/.ivy2/local/), type the following command:

```
$ sbt publishLocal
```

After publishing process, you can link to *ReadLogic* in you sbt project (e.g., inside the build.sbt file) by adding the following dependency:

```
libraryDependencies += "com.github.anskarl" %% "readlogic" % "0.1"
```