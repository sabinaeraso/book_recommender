# BookMatch!

A Book Recommendation Software

BookMatch is a book recommendation app run through command line which presents the user with one book recommendation at a time.
Utilizing the OpenLibary and Google Books RestAPIs, Book Match implements a similarity algorithm which finds book recommendations based on the user's favorite book and opinions on subsequent recommendations.

The user starts with inputting their favorite book and then has the option to choose "interested", "uninterested", "read and liked", or "read and did not enjoy" on the following recommendations, 
allowing the program to learn more about the user's interests to make more accurate recommendations.

BookMatch exists as a fun and simplistic way for a user to recieve book recommendations while doing minimal work searching for books themselves. It is a fun project intended to help avid readers and new readers alike. 

The program utilizes Open Library and Google Books RestAPIs, along with the Ocaml libraries Fzf, Yojson, Async_interactive. 

# How to Setup / Run Project

## Setup

To download the dependencies users should run the below commands.

```
$ opam install async_interactive fzf
```

```
$ sudo apt install fzf # to install the fzf binary also
```

Users must also create / specify a directory in which they want the cache to be stored. The program will take the path from the directory it is being run to this directory as an optional argument and the program will not run in the path is inaccurate. If this optional argument is not passed the program will assume there is a "cache/" directory within the directory the program is being run

## Running

After downloading the depedencies and creating a directory for the cache, users should navigate to the book_recommender directory.

Users should use the below command to start the program!

```
$ dune exec ./bin/main.exe -- run recommend -path <path to the cache>
```

# Members

This project was brainstormed, designed and implemented by aw03 and sabinaeraso on Github