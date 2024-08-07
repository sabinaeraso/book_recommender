# BookMatch!

A Book Recommendation Software

BookMatch is a book recommendation app run through command line which presents the user a tinder-style approach to book recommendations.
Utilizing the OpenLibary and Google Books RestAPIs, Book Match implements a similarity algorithm which finds books similar to the user's favorite book.

The user starts with inputting their favorite book and then has the options to choose interested, uninterested or read on the following recommendations, 
allowing the program to learn more about the user's interests to make more accurate recommendations.

BookMatch exists as a fun and simplistic way for a user to recieve book recommendations while doing minimal work searching for books themselves. It is a fun project intended to help avid readers and new readers alike. 

The program utilizes Open Library and Google Books RestAPIs, along with the Ocaml libraries Fzf, Yojson, Async_interactive. 

# How to Run Project

To download the dependencies users should run the below commands.

```
$ opam install async_interactive
```

```
$ opam install fzf
```

After downloading the program and depedencies, users should navigate to the book_recommender directory.

Users should use the below command to start the program.

```
$ dune exec ./bin/main.exe -- run recommend
```

# Members

This project was brainstormed, designed and implemented by aw03 and sabinaeraso on Github