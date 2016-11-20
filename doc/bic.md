

# Module bic #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A simple module for [BIC](http://en.wikipedia.org/wiki/ISO_9362) manipulation.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fake-1">fake/1</a></td><td>
Generate a fake BIC code.</td></tr><tr><td valign="top"><a href="#is_valid-1">is_valid/1</a></td><td>
Validate a BIC code.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fake-1"></a>

### fake/1 ###

<pre><code>
fake(Country::binary()) -&gt; binary()
</code></pre>
<br />

Generate a fake BIC code

<a name="is_valid-1"></a>

### is_valid/1 ###

<pre><code>
is_valid(BIC::string() | binary()) -&gt; true | false
</code></pre>
<br />

Validate a BIC code

