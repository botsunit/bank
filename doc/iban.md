

# Module iban #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A simple module for [IBAN](http://en.wikipedia.org/wiki/International_Bank_Account_Number) manipulation.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fake-1">fake/1</a></td><td>
Generate a fake IBAN.</td></tr><tr><td valign="top"><a href="#flatten-1">flatten/1</a></td><td>
Flatten the given IBAN.</td></tr><tr><td valign="top"><a href="#is_valid-1">is_valid/1</a></td><td>
Validate an IBAN.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fake-1"></a>

### fake/1 ###

<pre><code>
fake(Country::binary()) -&gt; binary() | nomatch
</code></pre>
<br />

Generate a fake IBAN

<a name="flatten-1"></a>

### flatten/1 ###

<pre><code>
flatten(IBAN::binary()) -&gt; binary()
</code></pre>
<br />

Flatten the given IBAN

<a name="is_valid-1"></a>

### is_valid/1 ###

<pre><code>
is_valid(IBAN::string() | binary()) -&gt; true | false
</code></pre>
<br />

Validate an IBAN

