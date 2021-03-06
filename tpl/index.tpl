<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE html 
  PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta http-equiv="Content-Language" content="en-gb" />
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
    <title>The Glasgow Haskell Compiler</title>
    <link href="/css/bootstrap.css" rel="stylesheet" type="text/css" />
    <link href="/css/purple.css" rel="stylesheet" type="text/css" />
  </head>
  <body class="page-home">
    <div class="wrap">
      <nav class="navbar navbar-default">
        <div class="container">
          <div class="navbar-header">
            <a href="/" class="navbar-brand"><span class="logo"></span>GHC</a>
          </div>
          <div class="collapse navbar-collapse">
            <ul class="nav navbar-nav">
              <li><a href="/downloads" class="active">Downloads</a></li>
              <li><a href="/community">Community</a></li>
              <li><a href="/documentation">Documentation</a></li>
              <li><a href="/news">News</a></li>
            </ul>
          </div>
        </div>
      </nav>
    </div>
  <div class="header"><div class=" container "><div class=" row "><div class=" span6 col-md-6"><div class="branding">
    <span class="name">The Glasgow Haskell Compiler</span><span class="summary">An advanced purely-functional programming language</span></div></div><div class=" span6 col-md-6"><div class="branding"><span class="tag">Download the latest</span><br/>
    <br>
    <a class="btn btn-default" href="#" role="button">GHC 7.10.3</a>
        </div></div></div></div></div>


    <div id="community-wrapper">
      <div style="background-image: url(/img/community.jpg)" class="community">
        <div id="tagline" class=" container ">
          <div class=" row ">
            <div class=" span8 col-md-8">
              <h1>An open source community effort for over 20 years</h1>
              <p class="learn-more"><a href="/community">Learn more</a></p>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="features">
      <div class="container">
        <h1>What is GHC?</h1>
        <p>GHC is a state-of-the-art, open source, compiler and
        interactive environment for the functional language <a href="http://www.haskell.org/">Haskell</a>.  Highlights:
      </p>
        <div class="row">
          <div class="span6 col-md-6">
            <h2>Extensions</h2>
            <p>GHC supports the entire <b><a href="http://www.haskell.org/haskellwiki/Definition">Haskell 2010 language</a></b> plus a wide
          variety of <b><a href="docs/latest/html/users_guide/ghc-language-features.html">extensions</a></b>.</p>
          </div>
          <div class="span6 col-md-6">
            <h2>Parallel Computing</h2>
            <p>GHC has particularly good support for <b><a
          href="docs/latest/html/libraries/base/Control-Concurrent.html">concurrency</a></b>
          and <b><a
            href="docs/latest/html/users_guide/using-smp.html">parallelism</a></b>,
          including support for <b><a
            href="http://hackage.haskell.org/package/stm">Software
          Transactional Memory (STM)</a></b>.</p>
          </div>
        </div>
        <div class="row">
          <div class="span6 col-md-6">
            <h2>Performance</h2>
            <p>GHC generates fast code, particularly for concurrent
          programs.  Take a look at GHC's performance on <b><a
            href="http://shootout.alioth.debian.org/">The Computer Language
          Benchmarks Game</a></b>.</p>
          </div>
          <div class="span6 col-md-6">
            <h2>Platform Support</h2>
            <p>GHC works on several <b><a href="http://hackage.haskell.org/trac/ghc/wiki/Platforms">platforms</a></b>
          including Windows, Mac, Linux, most
          varieties of Unix, and several different processor
          architectures.  There are detailed <b><a href="http://hackage.haskell.org/trac/ghc/wiki/Building/Porting">instructions</a></b>
          for porting GHC to a new platform.</p>
          </div>
        </div>
        <div class="row">
          <div class="span6 col-md-6">
            <h2>Optimisation</h2>
            <p>GHC has extensive <b><a href="docs/latest/html/users_guide/options-optimise.html">optimisation</a></b> capabilities, including
          inter-module optimisation.</p>
          </div>
          <div class="span6 col-md-6">
            <h2>Compilation Environments</h2>
            <p>GHC compiles Haskell code either directly to native
          code or using <a href="http://llvm.org/">LLVM</a> as a
          back-end.  GHC can also generate C code as an intermediate
          target for porting to new platforms. The <b><a href="docs/latest/html/users_guide/ghci.html">interactive environment</a></b> compiles Haskell to bytecode, and supports
          execution of mixed bytecode/compiled programs.</p>
          </div>
        </div>
        <div class="row">
          <div class="span6 col-md-6">
            <h2>Profiling</h2>
            <p><b><a href="docs/latest/html/users_guide/profiling.html">Profiling</a></b> is supported, both by time/allocation and various kinds of heap profiling.</p>
          </div>
          <div class="span6 col-md-6">
            <h2>Libraries</h2>
            <p>GHC comes with several
          <b><a href="docs/latest/html/libraries/index.html">libraries</a></b>, and thousands more are available on <a href="http://hackage.haskell.org/packages/hackage.html">Hackage</a>.</p>
          </div>
        </div>
      </div>
    </div>
    $side$
    <h2>Latest News</h2>
    <dl>
      <dt><strong>27 March 2015</strong></dt>
      <dd>GHC 7.10.1 Released! [<a href="download_ghc_7_10_1">download</a>]</dd>
      <dt><strong>23 December 2014</strong></dt>
      <dd>GHC 7.8.4 Released! [<a href="download_ghc_7_8_4">download</a>]</dd>
      <dt><strong>11 July 2014</strong></dt>
      <dd>GHC 7.8.3 Released! [<a href="download_ghc_7_8_3">download</a>]</dd>
      <dt><strong>12 April 2014</strong></dt>
      <dd>GHC 7.8.2 Released! [<a href="download_ghc_7_8_2">download</a>]</dd>
      <dt><strong>9 April 2014</strong></dt>
      <dd>GHC 7.8.1 Released! [<a href="download_ghc_7_8_1">download</a>]</dd>
      <dt><strong>21 April 2013</strong></dt>
      <dd>GHC 7.6.3 Released! [<a href="download_ghc_7_6_3">download</a>]</dd>
      <dt><strong>29 January 2013</strong></dt>
      <dd>GHC 7.6.2 Released! [<a href="download_ghc_7_6_2">download</a>]</dd>
      <dt><strong>6 September 2012</strong></dt>
      <dd>GHC 7.6.1 Released! [<a href="download_ghc_7_6_1">download</a>]</dd>
      <dt><strong>10 June 2012</strong></dt>
      <dd>GHC 7.4.2 Released! [<a href="download_ghc_7_4_2">download</a>]</dd>
      <dt><strong>2 February 2012</strong></dt>
      <dd>GHC 7.4.1 Released! [<a href="download_ghc_7_4_1">download</a>]</dd>
      <dt><strong>11 November 2011</strong></dt>
      <dd>GHC 7.2.2 Released! [<a href="download_ghc_7_2_2">download</a>]</dd>
      <dt><strong>9 August 2011</strong></dt>
      <dd>GHC 7.2.1 Released! [<a href="download_ghc_7_2_1">download</a>]</dd>
      <dt><strong>15 June 2011</strong></dt>
      <dd>GHC 7.0.4 Released! [<a href="download_ghc_7_0_4">download</a>]</dd>
      <dt><strong>27 March 2011</strong></dt>
      <dd>GHC 7.0.3 Released! [<a href="download_ghc_7_0_3">download</a>]</dd>
      <dt><strong>3 March 2011</strong></dt>
      <dd>GHC 7.0.2 Released! [<a href="download_ghc_7_0_2">download</a>]</dd>
      <dt><strong>16 November 2010</strong></dt>
      <dd>GHC 7.0.1 Released! [<a href="download_ghc_7_0_1">download</a>]</dd>
      <dt><strong>12 June 2010</strong></dt>
      <dd>GHC 6.12.3 Released! [<a href="download_ghc_6_12_3">download</a>]</dd>
      <dt><strong>22 April 2010</strong></dt>
      <dd>GHC 6.12.2 Released! [<a href="download_ghc_6_12_2">download</a>]</dd>
      <dt><strong>14 December 2009</strong></dt>
      <dd>GHC 6.12.1 Released! [<a href="download_ghc_6_12_1">download</a>]</dd>
      <dt><strong>16 July 2009</strong></dt>
      <dd>GHC 6.10.4 Released! [<a href="download_ghc_6_10_4">download</a>]</dd>
      <dt><strong>9 May 2009</strong></dt>
      <dd>GHC 6.10.3 Released! [<a href="download_ghc_6_10_3">download</a>]</dd>
      <dt><strong>1 April 2009</strong></dt>
      <dd>GHC 6.10.2 Released! [<a href="download_ghc_6_10_2">download</a>]</dd>
      <dt><strong>4 November 2008</strong></dt>
      <dd>GHC 6.10.1 Released! [<a href="download_ghc_6_10_1">download</a>]</dd>
    </dl>
    <h2>Contribute</h2>
    <div class="body">

      <p>GHC is heavily dependent on its users and <a href="contributors">contributors</a>.
        Please come and join the <a href="docs/latest/html/users_guide/mailing-lists-GHC.html">mailing
        lists</a> and send us your comments, suggestions, bug reports
        and contributions!
      </p>
    </div>
   $footer$
  </body>
</html>
