# Course on Compiler Design.

[![Build Status][status]](https://app.shippable.com/bitbucket/piyush-kurur/compilers)

This is the repository for the first course on compiler construction
that I teach at IIT Palakkad. The material available here are relevant
to the course CS3300 titled "Compiler Design" and its associated
laboratory CS3310 titled "Compiler Design Laboratory". Here you will
find notes, code samples, and other information that are relevant for
this course. Students are encouraged to use the wiki that is available
with this repository for information. We also encourage responsible
editing of the wiki.

Some important links

* [Course Repository]

* [Course Wiki]

* [Issue tracker]

For any problems regarding the course please use the issue
tracker. Remember that the issue tracker is public and any one can see
the discussions therein. This has privacy implications and you might
not want to discuss things like your grade.

## Testing your setup

We have included some tests in the source files. You can run this
using the the make command. This will ensure that you have setup your
machine correctly with the correct softwares for this course. These
tests are run by the [Shippable CI][shippable] every pushes into this
repository.

```
make test   # run the tests on the sample programs here.
make clean  # cleanup the directories of temporary files.

```

The [shippable builds][shippable] are run on a docker container which
is build out of the Dockerfile given in this directory.

[status]: <https://api.shippable.com/projects/59800285202dac07006dad2e/badge?branch=master> "Build Status"
[Course Repository]: <https://bitbucket.org/piyush-kurur/compilers>
[Course Wiki]:       <https://bitbucket.org/piyush-kurur/compilers/wiki/Home>
[Issue tracker]:     <https://bitbucket.org/piyush-kurur/compilers/issues>
[shippable]: <https://app.shippable.com/bitbucket/piyush-kurur/compilers/> "Shippable CI page"
