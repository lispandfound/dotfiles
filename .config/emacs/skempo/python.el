(skempo-define-tempo ifmain (:tag t :abbrev t :mode (python-mode))
  "if __name__ == '__main__':" n> r> p)

(skempo-define-skeleton pyproject ()
  nil
  "[build-system]" \n
  "requires = [\"setuptools\",\"setuptools-scm\"]" \n
  "build-backend = \"setuptools.build_meta\"" \n \n
  "[project]" \n
  "name = \"" (skeleton-read "Name: ") "\"" \n
  "authors = [{name = \"QuakeCoRE\"}]" \n
  "description = \"" (skeleton-read "Description: ") "\"" \n
  "readme = \"README.md\"" \n
  "requires-python = \">=3.7\"" \n
  "dynamic = [\"dependencies\", \"version\"]" \n \n
  "[tool.setuptools.dynamic]" \n
  "dependencies = {file = [\"requirements.txt\"]}")
