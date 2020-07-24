import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="idf",
    version="0.0.1",
    author="Jonathan Schilling",
    author_email="jonathan.schilling@mail.de",
    description="Interface Definition Framework: specify how codes talk to each other and auto-generate corresponding code.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/jonathanschilling/idf",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3',
    install_requires=['numpy'],
)

