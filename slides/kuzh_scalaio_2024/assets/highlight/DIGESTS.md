## Subresource Integrity

If you are loading Highlight.js via CDN you may wish to use [Subresource Integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity) to guarantee that you are using a legimitate build of the library.

To do this you simply need to add the `integrity` attribute for each JavaScript file you download via CDN. These digests are used by the browser to confirm the files downloaded have not been modified.

```html
<script
  src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.10.0/highlight.min.js"
  integrity="sha384-pGqTJHE/m20W4oDrfxTVzOutpMhjK3uP/0lReY0Jq/KInpuJSXUnk4WAYbciCLqT"></script>
<!-- including any other grammars you might need to load -->
<script
  src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.10.0/languages/go.min.js"
  integrity="sha384-Mtb4EH3R9NMDME1sPQALOYR8KGqwrXAtmc6XGxDd0XaXB23irPKsuET0JjZt5utI"></script>
```

The full list of digests for every file can be found below.

### Digests

```
sha384-LZ+YGF0Xve9sHzC9G37Kc14gDC6lfDpny2hggVJVfHb+OsTEXgMGLmAWUJzi4YRC /es/languages/rust.js
sha384-/ktfWRgwL+kZAZeeXDl9mwkD/3atjwjkzLCCoSHtME7MzP87wMhUmNUZ83AoqYx2 /es/languages/rust.min.js
sha384-bJFk2tsVn5F8YUHsonGtmJRBQDWY5KfeY1yoF/KyRKPSe27EXGZozBtwNVK2ZBE/ /es/languages/scala.js
sha384-Ww9na6pFMpl0LFINRDLDfPQf6xtS31SnqKhTud2UgsqMjEYdiU0UCoL6aNcUg0Gc /es/languages/scala.min.js
sha384-rCXn7K5j/lD2PrSex2XCqyLKap2Ibnsls0uQCx4ZaezI1FJ5RYvoWcsAl/v8SKlE /languages/rust.js
sha384-VTNxHMz2AmpHxzSm8SvRI0As5+wID2j2XJBFtWTic2iEK8WbXgR1fymVQS9S2DvY /languages/rust.min.js
sha384-/YpJjx5RnzUuJcVaJYIEU44EkoCD/KX5CiVM8Aj7wfkZj6EYw9g3CPauWCDI+ljG /languages/scala.js
sha384-SA6LxB7n4Y1G9cv1TTU9P9QCpQXJaNURCzOyoGjV/D9gkhcHYSsYERw8IVffZ8uD /languages/scala.min.js
sha384-U6FrMwHM968cF8zWMDGSOl3A5Rce9o8BSTnMgD4nXz9/wquuBAtC6W54vM9LuVAv /highlight.js
sha384-hgBzLLNkmYtmQ9lGjCzARAlV6levd4TDY5y8hr2KNPoxXEg5XZKuDISeyHivb1/x /highlight.min.js
```

