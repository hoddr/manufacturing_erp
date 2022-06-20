# Documentation

The following contains documentation both for intended design use as well as for devloper(s)
needing to update, fix, or modify the ERP. Sections intended for
developers should be quite clear, but a small denotation is added to help. Repo setup, the build process, and the remote server
setup is also detailed. In short, everything that a
competent developer/dev ops person should need to maintain and improve upon this code base.
Critical workflows will take documentation priority. While Haskell as a language choice
may be an initial learning barrier, advanced ideas were largely avoided in favor of lower-level abstractions.
Haskell's inherent type safety and compilation-driven behavior should
also make refactoring easier for future developers - a huge reason it was chosen from the start.
Coupled with documentation on the workflow and business critical logic sections,
the code should stand on its own, give or take a few quirks of languages and libraries.

## Documentation Topics
- [Repo Setup - Dev](./repo_setup.md)
- [Production/Deployment - Dev](./prod_builds.md)
- [Code Structure - Dev](./code_structure.md)
- [Routing](./router.md)
- [Controllers](./controllers.md)
- [Data Types](./data_types.md)

No outside use permitted without explicit written permission.

ERP
hoddr Feb 2022
