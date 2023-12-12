# This Site Now Has Uncool URLs

2023-11-23

W3C says that [cool URIs don't
change](https://www.w3.org/Provider/Style/URI), and one of the
suggestions is to leave the file name extension out of the URL. So
"/about" instead of "/about.html".

[Hugo](https://gohugo.io/) also follows this advise and generates what
it calls "pretty" URLs by default. So if you have "content/about.md",
it will generate "/about/" as the URL path, though there is an option
called
"[uglyURLs](https://gohugo.io/content-management/urls/#appearance)"
you can turn on to change it to generate "/about.html".

Cloudflare Pages is taking an even more opinionated approach
([currently not
configurable](https://community.cloudflare.com/t/prevent-truncating-and-removal-of-page-name-extensions/388845)),
so if your site has "/about.html" and when a user visits that,
Cloudflare will remove the extension and does a [308 permanent
redirect](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/308)
to "/about".

It's generally good advice, as hiding the file extension means hiding
away the implementation details, giving the server the flexibility to
change how the page is generated whenever it wants, it could be static
html files today, tomorrow it could be xhtml, or dynamically
generated.

However, as one may notice based of the URLs on this site, I have done
quite the opposite, where every link has a file extension, because of
the personal desire of wanting to make this site as static as
possible, where everything is a file and is statically/relatively
linked together. The end result is that it is now truly server-less,
you can download the whole thing and open the local pages with a
browser, and it will look exactly the same, with all links working
correctly, without serving it through a server. This is actually how I
use it too when writing.
