---
layout: post
title: N-Grams Scraper
categories: [web-app]
tags: [web-app]
thumb: http://placehold.it/40x40
caption: Builds a profile of the competition's website so you understand how to compete
---

### Synopsis
An `n-gram` is a contiguous string of words from some sentence. Ex: in `"Growmobily is Awesome"`, there are two 2-grams: `["Growmobily is", "is awesome"]`. Human speech/writing follows patterns where certain n-grams show up more frequently than others. If you analyze a competitors website, you can get a feel for what their main message is, and what customers they're targeting.

### Interesting points
The frontend displays data to end users, and allows them to post-process+download it. Useful. Boring to talk about.

The backend *is* interesting. It takes a website, searches deep (a fun algorithmy problem), and processes the results into a table of n-grams. It maintains a live connection to the frontend, so the user can analyze results real-time, as they're flowing in.
