# Graded browser support

We are committed to making Crowbar as accessible and consistent as we can.
However, it is not practical for us to support every available operating system
and web browser combination. Thus we have adopted the following graded browser
and operating system support policy, inspired by [Yahoo's GBS]
(http://yuilibrary.com/yui/docs/tutorials/gbs/).

Browser support grades:

| Grade | QA  | Bug priority         | Browser and version combinations |
|-------|-----|----------------------|----------------------------------|
| A     | Yes | Normal               | Current and previous major releases of Firefox, Firefox ESR (used in SLES/D), Chrome and Internet Explorer. |
| B     | No  | Low, likely WONT_FIX | Current major release of Safari. |
| C     | No  | WONT_FIX             | All browser and version combinations that are not in grades A or B. This includes old versions of A grade browsers and A grade browsers on unsupported operating systems. |

This does not mean that Crowbar will only work on A-grade browsers - we expect
it to work in all modern browsers and do make an effort to provide graceful
degradation and progressive enhancements. It simply means that A-grade browsers
are what we focus our development and QA efforts on, so those are what we
recommend for the optimal user experience.

Mobile browsers running on current versions of Android and iOS should generally
work as well, although certain features like mouse-over pop-ups will not work
on touch devices.

The following table provides an example of the major browser and version
combinations, the corresponding graded support, and their respective release
cycles.

> Note: All dates are in the yyyy-mm-dd format.

<table>
  <tr>
    <th rowspan='2'>Browser</th>
    <th rowspan='2'>Release cycle</th>
    <th colspan='3'>Current major release</th>
    <th colspan='3'>Previous major release</th>
  </tr>
  <tr>
    <th>Grade</th>
    <th>Version</th>
    <th>Date</th>
    <th>Grade</th>
    <th>Version</th>
    <th>Date</th>
  </tr>
  <tr>
    <td>Mozilla Firefox</td>
    <td><a href="https://wiki.mozilla.org/Releases#Previous_Releases">~1 month</a></td>
    <td>A</td>
    <td>17.0</td>
    <td>2012-11-20</td>
    <td>A</td>
    <td>16.0</td>
    <td>2012-10-09</td>
  </tr>
  <tr>
    <td>Mozilla Firefox ESR</td>
    <td><a href="http://www.mozilla.org/en-US/firefox/organizations/faq/">1 year</a></td>
    <td>A</td>
    <td>17.0 ESR</td>
    <td>2012-11-20</td>
    <td>A</td>
    <td>10.0 ESR</td>
    <td>2012-01-31</td>
  </tr>
  <tr>
    <td>Google Chrome</td>
    <td><a href="http://en.wikipedia.org/wiki/Google_Chrome">1-2 months</a></td>
    <td>A</td>
    <td>23.0.1271</td>
    <td>2012-11-06</td>
    <td>A</td>
    <td>22.0.1229</td>
    <td>2012-09-25</td>
  </tr>
  <tr>
    <td>Microsoft Internet Explorer</td>
    <td><a href="http://en.wikipedia.org/wiki/Internet_Explorer">Not time based</a></td>
    <td>A</td>
    <td>10</td>
    <td>2012-10-26</td>
    <td>A</td>
    <td>9</td>
    <td>2011-03-14</td>
  </tr>
  <tr>
    <td>Apple Safari</td>
    <td><a href="http://en.wikipedia.org/wiki/Safari_(web_browser)">Not time based</a></td>
    <td>B</td>
    <td>6</td>
    <td>2012-07-25</td>
    <td>C</td>
    <td>5</td>
    <td>2010-06-07</td>
  </tr>
</table>

The operating systems that the browsers run on is also a factor. We use the
same grade system as the browsers.

A-grade operating systems:
* Linux (latest supported SUSE desktop versions) - openSUSE 12.2, SLED11 SP2
* Microsoft Windows (latest two versions) - Windows 7, Windows 8

B-grade operating systems:
* Apple Mac OS X (latest two versions) - Lion (10.7), Mountain Lion (10.8)

C-grade operating systems:
* Everything not in A or B grade

Both [cookies](http://en.wikipedia.org/wiki/HTTP_cookie) and
[JavaScript](en.wikipedia.org/wiki/JavaScript) must be enabled for the site
to function properly.

Reference browser support policies in other projects:
[noVNC](https://github.com/kanaka/noVNC/wiki/Browser-support),
[Twitter Bootstrap](https://github.com/twitter/bootstrap/wiki/Browser-Compatibility).
