---
layout: post
title: PriceTracker.co
categories: [web-app]
tags: [web-app]
thumb: assets/media/portfolio/thumb/pricetracker.png
caption: Track Amazon prices, buy stuff when the price dips!
---

![]({{ site.url }}/assets/media/portfolio/pricetracker01.png){:height="300px"} | ![]({{ site.url }}/assets/media/portfolio/pricetracker02.png){:height="300px"}

### Synopsis
If you've ever mentally followed products on Amazon, you've noticed that the prices fluctuate. Like, a lot. This app takes advantage of the fact to help people buy products for cheaper.

### Interesting points
There's a lot going on to keep that frontend looking so simple, and we built the entire product, from back to front. It polls Amazon's API to get data, keeps track of it in a MongoDB, and allows users to poll it to run mini "reports" they can use to judge historical prices.

It's hosted for free on Heroku (note: that explains, the laggy startup time, followed by normal load times after it wakes up).
