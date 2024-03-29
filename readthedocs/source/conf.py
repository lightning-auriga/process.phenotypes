import sphinx_rtd_theme

copyright = "2022, B. Ballew, C. Auriga, 54gene"

project = "process.phenotypes"
version = "1.1.0"

templates_path = ['_templates']
html_static_path = ['_static']
html_css_files = ['custom.css']

extensions = [
    'myst_parser',
    'sphinx_rtd_theme',
]

html_theme = "sphinx_rtd_theme"

html_logo = "images/54gene_logo.png"
