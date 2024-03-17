## Generation of documentation

To generate the documentation, you will need [this fork](https://github.com/circular17/pasdoc) of pasdoc for it to work properly. The changes may be integrated in the future in the release of [pasdoc](https://github.com/pasdoc/pasdoc).

Compile the console version of pasdoc and copy into the [/doc](https://github.com/bgrabitmap/bgrabitmap/tree/dev-bgrabitmap/doc) folder of bgrabitmap. Then run the script called `generate.sh` (on Linux and MacOS). There is as well a batch file `generate.bat` for Windows though it doesn't generate the charts.

The charts are generated using [Graphviz](https://graphviz.org/) with its `dot` command. Some units and classes are from the `.dot` files before generating the chart in order to make it readable. They are accessible in the generated HTML documentation with the left panel.

The scripts generates the HTML documentation in a subfolder called **web**. It is not archived in this repository, considering it would take up space, is bound to be regenerated anyway and will be stored in the [documentation website](https://github.com/bgrabitmap/bgrabitmap.github.io).
