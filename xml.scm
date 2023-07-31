; Evaluation of this file yields an HTML document
; $Id: xml.scm,v 2.23 2022/05/03 06:23:03 oleg Exp oleg $

(define Content
'(html:begin
  (Header
   (title "XML and Scheme")
   (description "Representing, authoring, querying and transforming markup data in Scheme; XML notation for a programming language")
   (Date-Revision-yyyymmdd "20140507")
   (Date-Creation-yyyymmdd "20010125")
   (keywords "XML, XML parsing, XML Infoset, XPath, SAX, SXML, XSLT, Scheme, HTML composition, HTML authoring")
   (AuthorAddress "oleg-at-okmij.org")
   (long-title "XML and Scheme")
   (Links
    (start "index.html" (title "Scheme Hash"))
    (contents "../README.html")
    (prev "web.html")
    (next "SSAX-benchmark-1.html")
    (top "index.html")
    (home "http://okmij.org/ftp/")))

  (body
   (navbar)
   (page-title)

   ; (TOC)
   (h2 "Tools: SSAX, SXML, SXPath, SXSLT")
   (ul
    (li (local-ref "XML-parser"
		  "A functional-style framework to parse XML documents"))
    (li (local-ref "SXML-spec"))
    (li (local-ref "SXPath" "SXPath")
	"-- SXML query language, XPath implementation")
    (li (local-ref "SXSLT"))
    (li (local-ref "typed-SXML"))
    )

   (h2 "Applications, Examples, Sample Code")
   (ul
    (li "Authoring of web pages, XML documents, and (PDF) papers"
	(ul
	 (li (local-ref "XML-authoring"
			"SXML as a higher-order, more expressive
markup language"))
	 (li (local-ref "Math-authoring"))
	 (li (local-ref "SXML-diff"))
	 (li (local-ref "SXML-as-database")))
	)

    (li (local-ref "literate-DTD"))
   
   (li "Applications of the SSAX parsing toolkit"
       (ul
	(li (local-ref "SSAX-parsing-examples"))
	(li "parsing and unparsing of a namespace-rich XML document: DAML/RDF")
	(li (local-ref "SSAX-pull"))
	(li (local-ref "parent-ptr"))
	(li (local-ref "validation"))
	(li (local-ref "HTML-parser"))
	))

   (li (local-ref "SXSLT-examples"))

   
   )

   (h2 (local-ref "Papers"))
   (h2 (a (@ (href
	      "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))
	   "SSAX-SXML Mailing list")
       (n_) (n_)
       (a (@ (href
	      "http://ssax.sourceforge.net/"))
	   "SSAX-SXML SourceForge Project"))

   (h2 "Miscellanea")
   (ul
    (li (local-ref "PCDATA-ANY"))
    (li (local-ref "SOAP-SXML"))
    (li (local-ref "eval-SXML" 
		   (em "Evaluating") " SXML"))
    (li (local-ref "executable-XML"))
    )

   (p (hr))

;	(br) (n_))

;------------------------------------------------------------------------

    (Description-unit "Papers" "Papers and Presentations"
     (body
      (p (a (@ (href "SXML-short-paper.html"))
	      "SXML short presentation") (br)
	      "A micro-talk presentation at the Workshop on Scheme and Functional Programming 2000. Montreal, September 17, 2000.")
      (p 
	(Cite "A better XML parser through functional programming")
	(URL "http://link.springer.de/link/service/series/0558/tocs/t2257.htm")
	(br)
	(fileref "../papers/XML-parsing.ps.gz")
	(fileref "../papers/XML-parsing-talk.ps.gz")
	"The paper is published in LNCS volume 2257 with the Copyright
held by Springer-Verlag. The file " (code "XML-parsing.ps.gz") " is a
local copy of that copyrighted paper. The file " (code
"XML-parsing-talk.ps.gz") " is the transcript of a presentation at PADL
2002, the 4th International Symposium on Practical Aspects of
Declarative Languages. The topic of the presentation was the
derivation of the SSAX API and the comparison of SSAX with other
functional-style XML parsers and with Expat.")
      (p (local-ref "SXML-spec" (Cite "SXML Specification") 
		    ", in SXML/HTML/LaTeX/PDF")
	 (br) "ACM SIGPLAN Notices, v.37(6), pp. 52-58, June 2002.")
      (p 
	(Cite "XML, XPath, XSLT implementations as SXML, SXPath, and SXSLT")
	(br)
	(fileref "../papers/SXs.pdf")
	(fileref "../papers/SXs-talk.pdf")
	 "A paper and a transcript of a complementary talk presented at the International Lisp Conference 2002, October 27-31, 2002."
	 )
      (p 
	(Cite "SXSLT: Manipulation Language for XML") 
	(URL "http://link.springer.de/link/service/series/0558/tocs/t2562.htm")
	(br)
	(fileref "../papers/SXSLT.ps.gz")
	(fileref "../papers/SXSLT-talk.pdf")
	"The paper (with Shriram Krishnamurthi) is published in LNCS
volume 2562 with the Copyright held by Springer-Verlag. The file
" (code "SXSLT.ps.gz") " is a local copy of that copyrighted
paper. The file " (code "SXSLT-talk.pdf") " is the transcript of a
presentation at PADL 2003, the 5th International Symposium on
Practical Aspects of Declarative Languages. The paper introduces SXSLT
and compares it with XSLT.  Our experience and user comments show that
SXSLT is expressive and easy to use. We argue that this outcome is a
consequence of SXSLT providing right abstractions for XML
transformations, of being higher-order, declarative and extensible.")
     (p
       "Advanced SXLST tutorial" (br)
       (fileref "sxslt-advanced.scm")
       "This file describes common patterns of SXSLT on an interesting
example. We demonstrate higher-order tags, pre-order and post-order
transformations, re-writing of SXML elements in regular and special
ways, context-sensitive applications of re-writing rules, and
reflection. Although the file is technically a Scheme source code, it is
actually a tutorial. Of 357 lines in the file, 241 are comments and 24
are just blank lines."))  )


   (Description-unit "XML-parser"
     ("Functional XML parsing framework" (br)
      "SAX/DOM and SXML parsers with support for XML Namespaces and validation")
     (body 
      "The framework consists of a DOM/SXML parser, a SAX parser, and a
supporting library of lexing and parsing procedures. The procedures in
the package can be used separately to tokenize or parse various pieces
of XML documents. The framework supports XML Namespaces, character,
internal and external parsed entities, xml:space, attribute value
normalization, processing instructions and CDATA sections.  The
package includes a semi-validating " (em "SXML parser") ": a DOM-mode
parser that is an instantiation of a SAX parser (called SSAX)."
      (p
       "The parsing framework offers support for XML validation, to
the full or any user-specified degree. Framework's procedures by
themselves detect great many validation errors and almost all
well-formedness errors. Furthermore, a user is given a chance to set
his own handlers to capture the rest of the errors. Content is
validated given user-specified constraints, which the user can derive
from a DTD, from an XML schema, or from other competing doctype
specification formats.")
      (p
       "SSAX is a full-featured, algorithmically optimal,
pure-functional parser, which can act as a stream processor. SSAX is
an efficient SAX parser that is " (em "easy to use") ". SSAX minimizes
the amount of application-specific state that has to be shared among
user-supplied event handlers. SSAX makes the maintenance of an
application-specific element stack unnecessary, which eliminates
several classes of common bugs. SSAX is written in a pure-functional
subset of Scheme. Therefore, the event handlers are referentially
transparent, which makes them easier for a programmer to write and to
reason about.  The more expressive, reliable and easier to use
application interface for the event-driven XML parsing is the outcome
of implementing the parsing engine as an enhanced tree fold
combinator, which fully captures the control pattern of the
depth-first tree traversal.")
      )
     (platforms "The package has been tested on PLT Scheme (versions
103 and 200), Bigloo 2.4b, GambitC 3.0, Chicken, Guile, SCM, MIT
Scheme 7.5.2, Gauche, and Petite Chez Scheme 6.0a")
     (version "5.1, Sep 16, 2004")
     (references
      (p (a (@ (href "http://ssax.sourceforge.net"))
	    "ssax.sourceforge.net") (br)
	    "SSAX and SXML at SourceForge")
      (p (a (@ (href "http://sourceforge.net/cvs/?group_id=30687"))
	    "CVS tree") " at SourceForge" (br)
	    "The CVS tree includes the complete SSAX/SXML code with
all its dependencies, some documentation, validation tests, as well as
several " (em "sample applications, benchmarks and usable examples")
". You can browse the files in the CVS tree from any web browser.")
      (p (fileref "lib/SSAX.scm"
		  "Well-commented source code of the package. The code comes with an extensive set of self-tests, which verify not only the correct
behavior but also the detection of constraint violations."))
;       (p (local-ref "SXML-spec" "SXML Specification") (br)
; 	 "Definition of SXML: an instance of XML Infoset as
; S-expressions, an Abstract Syntax Tree of an XML document.")
      (p (a (@ (href "http://metapaper.net/xml/ssax/"))
	   "http://metapaper.net/xml/ssax/") (br)
	    "Current versions of the SSAX ports to various Scheme systems (including Bigloo, Guile, Chicken, and PLT Scheme). These versions are kindly
maintained by Kirill Lisovsky, whose efforts are greatly appreciated.")
      (local-ref "Papers")

      (p (a (@ (href "../Haskell/Iteratee/index.html#xml"))
	   "Haskell Iteratee version of the XML parsing framework"))

      (p (a (@ (href "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))
	    "SSAX/SXML mailing list"))
      (p (a (@ (href "SSAX-benchmark-1.html"))
	    "Comparing SSAX and Expat XML parsers in features and performance")
	 )
      (p (textref "SSAX-v4.txt"
		  "SAX/DOM/SXML parsers with support for XML Namespaces and validation")
	 "An article posted on comp.lang.scheme and comp.text.xml
newsgroups on Thu, 15 Mar 2001 19:31:16 GMT")
      (p (textref "SSAX-v1.txt"
		  "A simple XML _lexer_ in Scheme"
		  "Announcement of version 1 of SSAX. An article
posted on comp.lang.scheme and comp.text.xml newsgroups on Sun, 28 Nov
1999 23:12:50 GMT")))
    (requires
     (
      (a (@ (href "parsing.html"))
	 "Input parsing primitives"))
     )
    )


   (Description-unit "SXML-spec"
		     "SXML specification"
      (body
       (p "SXML is an abstract syntax tree of an XML document. SXML is also a
concrete representation of the XML Infoset in the form of
S-expressions. The generic tree structure of SXML lends itself to a
compact library of combinators for querying and transforming SXML.")
       (p "The master SXML specification file is written in SXML
itself. With appropriate stylesheets, the master file has been
converted to a web page and to a LaTeX file. The latter was processed
to a PDF document."))
      (version "3.0, Mar 12, 2004")
      (references
       (p (fileref "SXML.scm"
		   "The master file, written in SXML. The second part of the file is a stylesheet for rendering the specification in HTML."))
       (p (fileref "SXML.html"
		   "The specification web page, converted from SXML above."))
       (p (fileref "SXML-paper.tex"
		   "The specification in LaTeX, converted from SXML above."))
       (p (fileref "SXML-paper.scm"
		   "The conversion stylesheet, from SXML to LaTeX."))
       (p (fileref "../papers/SXML-paper.pdf"
		   "The specification in the PDF format."))
       (p "ACM SIGPLAN Notices, v.37(6), pp. 52-58, June 2002."))
)


   (Description-unit "SXPath"
      "SXPath -- SXML query language, XPath implementation"
     (body
      (p "SXPath is a query language for SXML, an instance of an XML
Information set (Infoset) in the form of s-expressions. On the other
hand, SXPath is an implementation of XPath: there exists a formal
re-writing from XPath to SXPath. Abbreviated-form SXPath expression
look the same as abbreviated-form XPath modulo delimiters and
parentheses. XPath underlies both XSLT and XPointer W3C
Recommendations.")
      (p "The full-form SXPath notation is a " (em "combinator library")
	 " for querying SXML. It is comprised of several primitive
converters that can be joined by a small number of powerful
combinators. Primitive and composite converters closely correspond to
XPath location paths. This makes SXPath an XPath's " (em "virtual
machine") ".")
      (p "The new version of SXPath.scm has more and better comments, more
converters and combinators. It supports " (code "following-sibling::")
" and " (code "preceding-sibling::") " axes of XPath. The better half
of the SXPath.scm file (lines 463 through 1154) is a built-in test
suite. The tests make sure that examples given in the XPath
Recommendation indeed work in SXPath. The regression tests show the
correspondence between full- and abbreviated-form XPath expressions,
and the corresponding notations of SXPath. One of the examples is to
illustrate and test the difference between " (code "//para[1]")
" and " (code "/descendant::para[1]") ".  The difference is explained in a
note to Section 2.5 of the XPath Recommendation.  After considerable
practice, I need to read the note only twice or thrice to remember
what this distinction is. SXPath somehow is capable of reproducing the
said difference."))
     (platforms "I have tested on Gambit-C 3.0, SCM 5d2 and Bigloo 2.4b")
     (version "3.5, Jan 12, 2001")
     (references
      (local-ref "Papers")
      (p (fileref "lib/SXPath.scm"
		  "Complete, very commented source code with included extensive validation tests.")))
     (requires
      ((a (@ (href "util.html#myenv")) "My own standard prelude")
       ))
)

   (Description-unit "SXSLT" 
     "SXML traversals and transformations"
     (body
      (p
	"SXSLT is a practical XML transformation language. It is a
" (em "higher-order") " pure functional transformation language based
on flexible tree traversals. SXSLT is implemented as a library in a
pure functional subset of Scheme. At the core of this library is a
function " (code "pre-post-order") ", which takes an SXML tree and and
a stylesheet, and returns a transformed tree."))
     (headline
      (code "pre-post-order:: TREE x BINDINGS -> TREE"))
     (body
      (p "where"
	 (verbatim
	  "type TREE = SXML-NODE | SXML-NODELIST"
	  "type BINDINGS = (BINDING ...)"
	  "type BINDING = (TRIGGER-SYMB *preorder* . HANDLER)"
	  "             | (TRIGGER-SYMB *macro* . HANDLER)"
	  "             | (TRIGGER-SYMB NEW-BINDINGS . HANDLER)"
	  "             | (TRIGGER-SYMB . HANDLER)"
	  "type TRIGGER-SYMB = SXML-NAME | *text* | *default*"
	  "type NEW-BINDINGS = BINDINGS"
	  "type HANDLER = TRIGGER-SYMB x TREE -> TREE"
	  ))
      (p
       "The function pre-post-order traverses a " (code "TREE") ",
which is either a " (code "SXML-NODE") " or " (code "SXML-NODELIST")
". For each " (code "SXML-NODE") " of the form " (code 
"(SXML-NAME SXML-NODE...)") " the function looks up an
association with the given " (code "SXML-NAME") " among its " (code
"BINDINGS") ". If failed, the function tries to locate a
" (code "*default*") " binding. It is an error if the latter
attempt fails as well.")
      (p "Having found a binding, the function pre-post-order first
checks whether the binding prescribes a pre-order traversal. If
this is the case, the handler is 'applied' to the current
node. Otherwise, the pre-post-order function first calls itself
recursively on each child of the current node, with " (code
"NEW-BINDINGS") " prepended to the current bindings.  The handler
is then applied to the head of the current node and the transformed
children. The result of the handler, which should also be an " (code
"TREE") ", becomes a " (code "SXML-NODE") " of the transformed
tree. If the current " (code "SXML-NODE") " is a text string, a
special " (code "*text*") " binding is looked up.")
      (p
	 "The " (code "NEW-BINDINGS") " that appears in
" (code "(TRIGGER-SYMB NEW-BINDINGS . HANDLER)") " has the same form as
" (code "BINDINGS") ". The " (code "NEW-BINDINGS") " is a local
stylesheet. It takes effect only when traversing the children of a
particular node (whose name is the " (code "TRIGGER-SYMBOL") "). The
bindings of " (code "NEW-BINDINGS") " override the bindings from the
\"parent\" stylesheet.")
      (p
	"A " (code "*macro*") " binding also specifies a pre-order
traversal. The handler of a " (code "*macro*") " binding is applied to
the current node. The result however does not become a node in the
output tree. Rather it is traversed again. A " (code "*macro*") "
binding is thus equivalent to a syntactic binding of Scheme. Although
" (code "*macro*") " could be written as a " (code "*preorder*") "
binding whose handler recursively invokes pre-post-order, this pattern
turns out to be quite frequent to justify its own notation. The word
'macro' highlights the computation of a fixpoint: we successively
re-write an SXML node into other nodes until we reach text fragments
representing the target HTML, XML or LaTeX document.")  )

     (headline
      (code "SRV:send-reply:: FRAGMENT x FRAGMENT ... -> BOOL"))
     (body
      (p "The " (code "FRAGMENT") "s are lists of strings, characters,
numbers, thunks, " (code "#f") " -- and other fragments. The function
traverses the tree of " (code "FRAGMENT") "s depth-first pre-order, writes out
strings and characters, executes thunks, and ignores " (code "#f") "
and " (code "'()") ".  The function returns " (code "#t") " if it
wrote anything."))

     (headline
      (code "replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST"))
     (body
      (p
       "The function replace-range traverses the " (code "FOREST") "
depth-first and cut/replaces ranges of nodes. Here")
      (verbatim
       "type FOREST = (NODE ...)"
       "type NODE = Atom | (Name . FOREST) | FOREST"
       "beg-pred:: NODE -> #f | FOREST"
       "end-pred:: NODE -> #f | FOREST")
      (p "The range of nodes to cut/replace is specified by two
predicates, " (code "beg-pred") " and " (code "end-pred") ". The nodes
that define a range do not have to have the same immediate parent, do
not have to be on the same level, and the end node of a range does not
even have to exist. The replace-range procedure removes nodes from the
beginning node of the range up to (but not including) the end node of
the range.  In addition, the beginning node of the range can be
replaced by a node or a list of nodes. The range of nodes is cut while
depth-first traversing the forest. If all branches of the node are cut
a node is removed as well.  The procedure can cut several
non-overlapping ranges from a forest."))

     (platforms "I have tested on Bigloo 2.4b, Gambit-C 3.0, and SCM 5d6")
     (version "1.8, March 24, 2003")
     (references
      (p (fileref "lib/SXML-tree-trans.scm"
		  "SXML expression tree transformers"))
      (p (fileref "tests/vSXML-tree-trans.scm"
		  "The validation code"))
      (p (local-ref "SXML-spec") (br)
	"The HTML conversion stylesheet in the master SXML
Specification file is a good example of " (code "*preorder*") ", local
stylesheet, and " (code "*macro*") " bindings.")
      (p (fileref "apply-templates.scm"
	       "Another approach to SXML transformations, in the
spirit of XSLT."))
      )
     (requires
      ((a (@ (href "util.html#myenv")) "My own standard prelude")
       ))
     )


   (Description-unit "typed-SXML"
     "HSXML: Typed SXML"
     (body
      (p 
	"This message reports on several experiments in Haskell to
ascertain plausibility, benefits and drawbacks of typed SXML in a
mainstream functional language. It turns out that Haskell as it is can
represent semi-structured data in " (em "SXML-conforming syntax") ", with the
extensible set of `tags' and " (em "statically enforced") " content model
restrictions. Querying, transforming and advanced rendering into HTML
and XML are possible. The experience of writing many
moderately large and complex
web pages in HSXML shows that the typed SXML can be used in
practice.")
       (p "The benefit of representing XML-like documents as a typed
data structure/Haskell code is static rejection of bad documents --
not only those with undeclared tags but also those where elements
appear in wrong contexts. Using HTML-like HSXML as an example, a
document with " (code "H1") " within the " (code "P") " element is
rejected as invalid. No HSXML transformation may introduce nested
"(code "Hn") " or " (code "P") " elements: the code will not compile
otherwise due to a type error. Thus the generated XML or HTML document
will not only be well-formed but also will satisfy some validity
constraints. Types also guide queries, transformations, and
rendering, making them context sensitive. The type inference of such
expressive types is possible; in fact, no type
annotations are needed in an HSXML document. 
It used to be that the type inference and type checking 
took time in GHC(i). The latest version is quite fast. Static typing
does not inhibit extensibility. The HSXML library user may always
define new `tags', new transformation rules and new contexts.")
      )
     (version "3.1, May 2014")
     (references
      (p (fileref "../Haskell/HSXML/Description.txt"
	   "HSXML Haskell library, sample code and descriptions"))
      (p "Takahiro Kido (shelarcy): Typed S-expression-based XML"
	(URL "http://page.freett.com/shelarcy/log/2012/diary_12.html#typed_s_expression_xml") (br)
	"Detailed and easy-to-understand introduction to HSXML (in Japanese)")
      (p (textref "../Haskell/Typed-SXML.txt"
	   "Typed SXML"
	   "An " (em "old") " message posted on the SSAX-SXML mailing list on
Mon, 18 Dec 2006 18:02:01 -0800 (PST). The message described version 1.0.
In the current version HSXML not longer uses square brackets and
looks almost the same as SXML: see the examples below."))
      (p "An example of authoring web pages in HSXML" (br)
	(fileref "../continuations/undelimited.hs"
	  "The master HSXML file") (br)
	(a (@ (href "../continuations/undelimited.html")) "The rendered page"))
;;       (p "A complex example of context-sensitive HSXLT transformations:
;; producing structurally distinct HTML and XML/RSS from the same master
;; file" (br)
;; 	(fileref "../ChangeLog.hs"
;; 	  "The master HSXML file: the log of changes on this site") (br)
;; 	(a (@ (href "../ChangeLog.html")) "The HTML version") (br)
;; 	(fileref "../rss.xml"
;; 	  "The flattened and rearranged RSS/XML version"))
      )
     )


;------------------------------------------------------------------------
   (p (hr))
   (h2 (@ (align "center")) "Applications, Examples, Sample Code")


   (Description-unit "XML-authoring"
     "HTML/XML authoring in Scheme"
     (body
      (p "The converse to XML parsing is SXML " (em "pretty-printing") ":
a translation from an abstract to a concrete XML syntax, to an XML or HTML
document. The pretty-printing makes it possible to author and
compose XML documents in their SXML form. SXML is more concise and
expressive than a raw markup language. SXML representing regular
Scheme code can be entered in any Scheme-sensitive editor. SXML as a
data structure -- a list -- can likewise be composed as a literal or
quasi-literal expression. Furthermore, SXML can be produced by regular
Scheme functions, which may make authoring more succinct, advanced,
and less tedious, as the code below illustrates.")
      (p "The pretty-printing consists of two passes. The first one
traverses an s-expression tree in " (em "post") "-order, and converts it
into a tree of HTML fragments. The tree of fragments, when flattened
and concatenated, yields the resulting HTML document. Of course there
is no need to flatten the tree and concatenate strings
explicitly. Rather, we traverse the tree of fragments depth-first and
write out fragments. In this approach, all character and other data,
once created, stay where they were originally allocated. " (em "No data
are ever copied.") " Functions such as " (code "string->number") " are
implied as well.")
      (p "The second pass, a pre-order traversal of a tree of fragments, is
performed by a function " (code "SRV:send-reply") ". Besides writing
out character data it encounters, the function executes thunks and
ignores " (code "'()") " and " (code "#f") ". This feature lets us,
for example, embed conditional forms into an s-expression
representing HTML:")
      (verbatim
       "(SXML->HTML"
       " `(p \"par1\" \"par2\" ,(and test (list \"par3\" \"par4\"))))"
       )
      "Depending on the value of the " (code "test") ", strings \"par3\" and
\"par4\" will be either both included, or both omitted from the
marked-up paragraph."
      (p "Here is a more meaningful example, which intelligently generates a
\"navigation bar\", a pair of links to the preceding and the following
slides. Obviously the first and the last page must have only one
navigation link.")
      (verbatim
       "(define (print-slide n max-count)"
       "  (SXML->HTML"
       "   `((h2 \"Slide number:\" ,n)     ; Note n is used in its native form"
       "     ,(and (positive? n)"
       " 	  `(a (@ (href \"base-url&slide=\" ,(- n 1))) \"prev\"))"
       "     ,(and (< (+ n 1) max-count)"
       " 	  `(a (@ (href \"base-url&slide=\" ,(+ n 1))) \"next\"))"
       "     (p \"the text of the slide\"))))"
      )

      (p "SXML makes composing XML or HTML documents even more convenient by
enabling higher-order \"tags\" -- just as LaTeX helps typeset documents
by offering higher-order \"macros\". LaTeX macros are eventually
expanded by TeX; SXML \"tags\" are evaluated by Scheme. The SXML
specification is an example of such an advanced composition.  The
SXML.html web page that describes SXML is actually written in SXML
itself.")

      "A better example of a " (em "logical") " markup is the following
excerpt from SXML.scm. It defines a " (code "*TOP*") " element of SXML:"
      (verbatim
       "(productions"
       "     (production 1"
       "       (nonterm \"TOP\")"
       "       ((sexp"
       "         (term-lit \"*TOP*\")"
       "         (ebnf-opt (nonterm \"aux-list\"))"
       "         (ebnf-* (nonterm \"PI\"))"
       "         (ebnf-* (nonterm \"comment\"))"
       "         (nonterm \"Element\")))))"
       )

      "When converted to HTML, the above fragment becomes"

      (verbatim
       "<table border=0 bgcolor=\"#f5dcb3\">"
       "<tr valign=top><td align=right><a name=\"prod-1\">[1]</a>&nbsp;</td>"
       "<td align=right><code>&lt;TOP&gt;</code></td>"
       "<td align=center><code> ::= </code></td>"
       "<td align=left><code><strong>(</strong> <em>*TOP*</em> &lt;aux-list&gt;? "
       "&lt;PI&gt;* &lt;comment&gt;* &lt;Element&gt; <strong>)</strong></code> </td>"
       "</tr></table>"
       )
      "In LaTeX, it does not look much better:"
      (verbatim
       "\\begin{tabular}{rrcp{2.8in}}"
       "{[}1{]} & \\texttt{<TOP>} &  $::=$ & \\texttt{\\textbf{(} {\\itshape *TOP*} <aux-list>? <PI>* <comment>* <Element> \\textbf{)} } \\\\"
       "\\end{tabular}"
       "\\\\"
       )

      "The SXML form appears most lucid. It was also easier to type,
especially given suitable Emacs key bindings, similar to those
designed for LAML."
      (p
       "Using SXML to express its grammar has another important
advantage: you can easily write a transformation or an SXPath query on
the whole SXML.scm to make sure that every " (code "nonterm") "
mentioned on the right-hand of some production appears on the
left-hand side of exactly one production. The SXML.scm code thus lends
itself not only to a flexible presentation to a human but to a formal
reasoning about as well.")
      (p
       "As a matter of fact, the ability to treat SXML as a \"code\"
or as \"data\" is used during SXML transformations. The expansion
process of a tag can re-scan the SXML document, with a different
stylesheet. That is how hierarchical tables of contents are
generated. Unlike LaTeX, you do not need to write auxiliary files and
do not need to re-run the document processor.")
      (p "The definition of " (code "SXML->HTML") " is given
below. The function also handles \"higher-level\" HTML tags,
for example, " (code "html:begin") ". These tags make HTML code even
more succinct and expressive, as the SXML code for this page clearly
shows. This page is authored in SXML. Local and site links are
generated automatically.")
      (p "The advantage of the present approach compared to LAML is
that we do not define any tag-related functions in the
interaction-environment. HTML tags appear to live in their own
\"namespace\". The function " (code "SXML->HTML") " handles the
arbitrary set of tags: we do " (em "not") " need to pre-define any tag or
attribute for it to work. On the other hand, if validation is desired,
we can introduce " (code "SXML->HTML") "'s bindings for all allowed tags,
and bind the " (code "*default*") " wildcard to a procedure that prints an
error. Therefore, our approach is just as flexible and expressive as
LAML. Furthermore, since SXML is also a data structure, you can store
it in a file. Or you you can store standard headers, footers,
etc. templates in files and \"pull\" them in when needed. This inclusion
of standard templates can be done lazily, for example, through a
higher-level " (code "include-and-process") " tag.")
      )

     (platforms "The package is tested on Gambit-C 3.0, SCM 5d6, Bigloo 2.4b and Petite Chez Scheme 6.0a")
     (version "1.5, Aug 10, 2002")
     (references
      (p (local-ref "SXML-spec") (br)
	 "An extensive example of HTML authoring 
in SXML, with a great number of high-level tags and the automatic
generation of a table of contents. Evaluation of SXML.scm file
generates SXML.html page.")
      (p (fileref "xml.scm"
		  "This present page in SXML. This is the master file for the present web page."))
      (p (fileref "lib/SXML-to-HTML.scm"
		  "Commented source code for the top-level HTML
transformer, SXML node handlers that generate the markup, and
auxiliary functions to quote \"bad\" characters."))
      (p (fileref "tests/vSXML-to-HTML.scm"
		  "Validation (sample) code"))
      (p (fileref "lib/SXML-to-HTML-ext.scm"
	    "Definitions of several functions and higher-order
SXML \"tags\" that are used to compose HTML pages on this web site. In LaTeX terms, this file is similar to article.cls."))
      (p (textref "LaXmL.txt"
		  "LaXmL: SXML as a higher-order markup language"
		  "An article posted on a comp.text.xml and comp.lang.scheme newsgroups on Thu, 5 Apr 2001 20:15:12 GMT."))
      (p (fileref "sxslt-advanced.scm")
	"An " (em "extensively") " commented example that transforms
an SHTML-like SXML document with nested sections into a web page with
recursively numbered sections and subsections. We use appropriate HTML
tags (H2, H3, ...)  to set off the title of the sections and
subsections. The example also generates a hierarchical table of
contents and places it before the first section.")
      (p
       (a (@ (href "web.html#search-mslib"))
	     "\"Active Server Pages\" in Scheme") (br)
; 	     "A " (code "get-sigmet-light") " ASP lets you search for aviation
; hazard advisories."
	     "An example of using the present HTML generation approach to write active server pages with complex stateful forms.")
      (p
       (a (@ (href "http://www.cs.auc.dk/~normark/laml/")) "LAML") (br)
       "Kurt Normark, \"Programming World Wide Web Pages in Scheme,\"
ACM SIGPLAN Notices, vol. 34, No. 12 - December 1999, pp. 37-46.")

      (p (local-ref "typed-SXML" "HTML authoring in the typed HSXML"))
      )
     (requires
      ((local-ref "SXSLT"))
       )
)

   (Description-unit "Math-authoring"
     "Writing LaTeX/PDF mathematical papers with SXML"
     (body
       (p
	"The following message discusses a fairly non-trivial example
of writing a theoretical computer science paper in SXML.  The message
shows many snippets of SXML and the corresponding LaTeX code. SXML
helped not only in transcribing (hand-written) mathematical
notation. The proofs themselves were " (em "conducted") " in
Emacs/SXML, rather than on a piece of paper. The Emacs/SXML
combination turns out a good assistant for equivalence-based proofs.")
       (p 
	 "The SXML->LaTeX formatter is quite distinct from SLaTeX.
We are not concerned with the typesetting of Scheme code. Rather,
our aim is to use Scheme notation as an easy-to-type and
easy-to-manipulate realization of mathematical notation. The end
result must be conventional mathematical notation, with " (code
"(lambda (x) (f g))") " typeset as " (code "$\\lambda x. f g$") ",
"(code "(compose f g h)") " as " (code "$f\\,\\cdot\\,g\\,\\cdot\\,h$") ",
"(code "(let ((x 1)) body)") " as " (code "$\\mathrm{let}\\,x=1
\\mathrm{in} body$") ", and the reduction of transformed A to
transformed B, " (code "(red* *A *B)") ", as " (code "$\\overline{A}
\\vartriangleright^{*} \\overline{B}$") ".")
       (p
	 "We also explain that the placement of parentheses is quite a
non-trivial problem. The message gives many an example.  To determine
whether a particular expression should be typeset enclosed in
parentheses, we need to know the property of the expression and the
property of the context. An expression may be enclosed
in an arbitrary number of decorators (i.e., subscripts, overlines,
etc), which complicates the matter. The typesetter is implemented as an
" (em "evaluator") " of the SXML notation using mixed call-by-name and
call-by-value rules and the explicit environment passing.")  )
     (version "Mar 23, 2005")
     (references
       (p
	 (textref "sxml-math.txt"
	   "Writing LaTeX/PDF mathematical papers with SXML"
	   "The message with many examples of writing and conducting
proofs in SXML. The messages also discusses the problem of
parentheses. The message was posted on the SSAX-SXML mailing list on
Tue, 26 Apr 2005 16:01:30 -0700."))
       (p
	 "Technical Report TR611, Department of Computer Science, Indiana
University, 2005"
      (URL 
	"http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR611"))
       (p
	 (fileref "impromptu-shift-tr.scm"
	"The master SXML file of the report" (br)
"The first part of the file is the SXML source for the report. The
second part is the formatter, from SXML to LaTeX (from which PDF was
eventually built)."))
       ))

   (Description-unit "SXML-diff"
     "Joint processing of two immutable SXML documents with update cursors"
     (body
       (p
	 "We give an example of traversing two SXML documents
``side-by-side'' and making pure functional updates to arbitrary
encountered nodes. The example illustrates the benefits of zipper --
an update cursor into an immutable data structure.")
       (p
	 "Our not-so-trivial example is detecting the differences in
character content within the corresponding paragraphs of two SXML
documents. We assume that two SXML documents in question are made of
the same number of paragraphs.  The paragraphs may be enclosed in
other elements such as " (code "div") ", chapters, etc., and that enclosing may
vary between the two documents. We disregard such differences when
comparing the documents. Paragraphs contain text, perhaps interspersed
with markup such as " (code "em") ", etc. We compare the text of the
respective paragraphs from two SXML documents and annotate the
paragraphs that differ.  We return two other SXML documents with so
annotated paragraphs.  We compare only the character content,
" (em "regardless") " of how that content is arranged into strings or
enclosed into additional markup elements.  For example, the following
two paragraphs are considered the same:")
       (verbatim
	 "(p \"This\" \" \" \"is a \" (strong \"simple\") \" example!\")"
	 "(p \"This is a simple \" (br) (em \"example\") \"!\")"
	 )
       "Our example is a non-trivial generalization of the ``same
fringe problem''."
       )

     (version "1.1, Aug 23, 2005")
     (references
       (p
	 (textref "streams-diff.txt"
	   "Stream-wise processing (differencing) of two SXML documents"
	   "The message with the explanation and examples of finding
differences in two SXML documents. The message was posted on the
SSAX-SXML mailing list on Tue, 23 Aug 2005 01:47:49 -0700."))
      (p (fileref "streams-diff.scm"
	   "The implementation of the zipper, of the paragraph
differencing function and of the examples."))
      (p
	(a (@ (href "misc.html#zipper"))
	  "Zipper in Scheme"))
      )
     )

 

   (Description-unit "literate-DTD"
     "Literate XML/DTD programming"
     (body
      (p "Designing an XML format for a particular domain involves more than
just creating a DTD for a collection of elements, attributes and
entities. An important part of the format is the documentation.
Ideally it is a hyperlinked document that explains the background, the
motivation, and the design principles; describes and cross-references
each tag and attribute; and shows representative examples of the
proposed markup. This article will demonstrate a tool that helps
design a specification and the documentation at the same time -- and to
keep them consistent.")
      (p "A literate design document should permit a transformation
into a well laid-out, easy-to-read hyperlinked user manual. A literate
design document should be easy to write. And yet the user manual
should be precise enough to allow automatical extraction of a formal
specification. We will demonstrate that SXML is rather suitable for
literate XML programming. SXML is similar to TeX, but is far easier to
write and read. SXML transformations do the job of \"weaving\" a
document type specification and of \"typesetting\" the user manual.")
      )
     (version "1.1, Apr 19, 2001")
     (references
      (p (textref "literate-DTD.txt"
		  "Literate XML/DTD programming"
		  "An article posted on a comp.text.xml and comp.lang.scheme newsgroups on Wed, 23 May 2001 04:26:31 GMT."))
      (p (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/JMGRIB.html"))
	    "JMGRIB.html") (br)
	 (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/JMGRIB.dtd"))
	    "JMGRIB.dtd") (br)
	 (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/JMGRIB.scm"))
	    "JMGRIB.scm") (br)
	    "JMGRIB: Joint METOC XML format for grid data. A User
Manual, DTD and the master file, i.e., literate DTD.")
      (p (local-ref "XML-authoring"))
      )
     )


   (Description-unit "SXML-as-database"
     "SXML as a normalized database"
     (body
      "S-expression-based files are handy not only for authoring Web
pages. They can be used to build XML documents. The following article
shows a real-life and " (em "less-trivial") " example of that.  It is
straightforward to convert " (code "<tag>data</tag>") " into " (code
"(tag \"data\")") " and vice versa. The SSAX parser and the SXML
manipulation tools can do that easily. However, exporting " (em
"relational") " sources into XML often runs into an impedance
mismatch: XML by its nature is a hierarchical database. We will show an
example of generating XML from s-expressions that involves
denormalizations, \"table joins\" and third-order tags. The
s-expression format turns out not only more understandable and
insightful, but more concise as well, by a factor of four.")
     (version "1.1, Apr 17, 2001")
     (references
      (p (textref "SXML-as-database.txt"
		  "Re: s-expressions as a file format?"
		  "An article posted on a comp.lang.scheme newsgroup on Wed, 29 Aug 2001 19:13:29 -0700."))
       (p
	 (local-ref "Papers" "SXs.pdf paper")
	 "Section 5 of that paper describes this example in great detail.")

      (p (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/Manifest.scm"))
	    "Manifest.scm") (note "from www.metnet.navy.mil") (br)
	 (a (@
	(href "http://diides.ncr.disa.mil/xmlreg/package_docs/Public/MET/OMF_package/997725059053/Manifest.scm"))
	    "Manifest.scm") (note "from DISA") (br)
	    (em "Normalized") " SXML database, which represents a
collection of OMF resources. The file Manifest.scm also includes a stylesheet that converts the SXML database to XML, and  \"joins\" s-expression-based tables in the process.")
      (p (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/Manifest.xml"))
	    "Manifest.xml") (note "from www.metnet.navy.mil") (br)
	 (a (@
	(href "http://diides.ncr.disa.mil/xmlreg/package_docs/Public/MET/OMF_package/997725059053/Manifest.xml"))
	    "Manifest.xml") (note "from DISA") (br)
	    (em "Denormalized") " XML database of the collection of OMF resources, in the DISA XML repository format.")
      (p (a (@
	(href "http://www.metnet.navy.mil/Metcast/XML/OMF.html"))
	    "OMF: Weather observations markup format"))
      (p (local-ref "XML-authoring"))
      )
     )

   (Description-unit "SXSLT-examples"
     "Complete examples of practical (context-sensitive) SXML Transformations"
     (body
      "The following examples demonstrate SXML transformations, from
trivial to advanced. The advanced examples exhibit context-sensitive
transformations: the result of the conversion of an SXML tag depends on
that tag's context, i.e., siblings, parents, or descendants.
We also demonstrate: higher-order tags, pre-order and
post-order transformations, re-writing of SXML elements in regular and
special ways. Finally, we illustrate SXSLT reflection: an ability of a rule
to query its own stylesheet and to re-apply the stylesheet with
\"modifications\".")
     (version "Dec 31, 2003")
     (references
      (p (fileref "apply-templates.scm"
	   "An example of a XSLT-like " (code "apply-templates") " form."))
      (p (fileref "sxml-db-conv.scm"
	   "Using higher-order transformers for context-sensitive
transformations. The example also shows the treatment of
XML Namespaces in SXML and the namespace-aware
pretty-printing of SXML into XML."))
      (p (fileref "pull-punct-sxml.scm"
		  "A context-sensitive recursive transformation
 		and breadth-first-like traversals of a SXML document."))
      (p (fileref "sxslt-advanced.scm")
	"An " (em "extensively") " commented example of SXSLT. 
The example transforms an SHTML-like SXML document with nested
sections into a web page with recursively numbered sections and
subsections. We use appropriate HTML tags (H2, H3, ...)  to set off
the title of the sections and subsections. The example also generates
a hierarchical table of contents and places it before the first
section.")
      (p (fileref "sxml-nesting-depth-label.scm")
	"Labeling nested sections of an SXML document by their nesting
depth. This is another example of traversing an SXML document with a
stylesheet that differs from node to node.")
       (p (fileref "sxml-to-sxml.scm") "Transforming SXML to SXML:
Composing SXML transformations.  The code introduces and tests a
version of pre-post-order that transforms an SXML document into a
" (em "strictly conformant") " SXML document. That is, the result of a
pre-post-order transformation can be queried with SXPath or
transformed again with SXSLT.")

      (p (a (@ (href "http://ssax.cvs.sourceforge.net/ssax/SSAX/examples/"))
	    "SSAX/SXML examples") (br)
	 "A directory of the examples, including the Makefile to run them."))
     )

        


   (Description-unit "SSAX-parsing-examples"
     "Complete examples of stream-wise (SAX) and DOM parsing"
     (body 
      "The following examples show how to use the SSAX library for a
process-as-we-parse, stream-wise (SAX) XML parsing. We also
illustrate a DOM parsing, and handling of namespace-rich documents. The
examples are the complete, ready to run code.")
     (version "Jan 6, 2007")

     (references
      (p (fileref "remove-markup.scm"
	   "Given an XML document, remove all markup. In other words,
compute the string-value of a DOM tree of the document, without
constructing the DOM tree."))
      (p (fileref "outline.scm"
	   "Pretty-print the structure of an XML document,
disregarding the character data. This is yet another example of
transforming an XML document on the fly, as we parse it. This example corresponds to outline.c of the Expat distribution."))

      (p (fileref "run-sxml.scm"
	   "A complete example of parsing an XML into SXML: DOM
parsing with SSAX."))
      (p (fileref "daml-parse-unparse.scm"
	   "Parsing and unparsing (and parsing again) of a
namespace-rich XML document: DAML/RDF. The example illustrates that
the parser and the pretty-printer maintain the invariant " (code
"PARSE . UNPARSE . PARSE == PARSE") ""))

      (p (fileref "ssax-extraction.scm"
	   "SSAX parsing of a " (em "large") " XML document and
recording only `interesting' parts, namely, the
content of CDATA sections from from a (small) subset of
elements. Because the document is large, it is important to avoid
reading it all in memory and retaining irrelevant data."))



      (p (a (@ (href 
		 "http://ssax.cvs.sourceforge.net/ssax/SSAX/examples/"))
	    "SSAX/SXML examples") (br)
	 "A directory of the examples, including the Makefile to run them.")
       (p (local-ref "parent-ptr")))
     )

   (Description-unit "HTML-parser"
     "Permissive parsing of perhaps invalid HTML"
     (body 
      (p "SSAX is more than an XML parser -- it is a library, which
includes lexers and parsers of various kinds. The library comes in
handy when we need to permissively parse (or better say, lex) HTML
documents or chunks, which may contain mismatched tags, unclosed tags,
or other imperfections.  Because browsers are so lax, many web
pages on the Internet are ill-coded. For example, the FrontPage
editor is notorious for creating such invalid sequences as " (code
"<b><i>text</b></i>") ".")
      (p "Because we accept even HTML with unmatched tags, we make no
attempt in this example to recover the structure. We faithfully record
all occurring tags and let the user sort out what matches
what. For example, the following ill-composed sample
document, which includes comments, parsed entities and attributes of
various kinds:")
      (verbatim
" <html><head><title> </title> <title> whatever </title></head>"
"  <body> <a href=\"url\">link</a> <p align=center> <ul compact"
"  style='aa'>"
"  <p> BLah <!-- comment <comment> --> <i> italic <b> bold <tt> end </i> still  &lt; bold </b>"
"  </body>"
"  <P> But not done yet...")
      "parses to a token stream:"
      (verbatim
       "(#(START html ()) #(START head ()) #(START title ())"
       "  \" \" #(END title) \" \" #(START title ()) \" whatever \" #(END title)"
       "  #(END head) \"\\n  \""
       "  #(START body ()) \" \" #(START a ((href . \"url\"))) \"link\" #(END a) \" \""
       "  #(START p ((align . \"center\"))) \" \""
       "  #(START ul ((compact . \"compact\") (style . \"aa\")))"
       "  \"\\n  \" #(START p ()) \" BLah  \" #(START i ()) \" italic \""
       "  #(START b ()) \" bold \" #(START tt ()) \" end \" #(END i)"
       "  \" still  < bold \" #(END b) \"\\n  \" #(END body)"
       " \"\\n  \" #(START P ()) \" But not done yet...\")")

      "The parser has handled " (code "&lt;") " and other parsed
entity references and fully preserved all the whitespace, including
newlines. The parser obviously supports the three possible styles of
HTML attributes: " (code "<tag name=\"value\">") ", " (code "<tag
name=value>") ", and " (code "<tag name>") ". Single quotes are also
allowed. Comments are silently ignored."
       (p "This is an example of using the SSAX library for tasks
other than XML parsing."))
     (version "1.1, Nov 3, 2001")
     (references
      (p (fileref "html-parser.scm"
		  "The source code of the parser and of the test to run the above example"))
      (p (a (@ (href "http://ssax.cvs.sourceforge.net/ssax/SSAX/examples/"))
	    "SSAX examples")
	 "A directory of SSAX examples, including the HTML parser, and the Makefile to run them.")
      (p
	"HtmlPrag: a permissive HTML parser that emits SXML, by Neil
W. Van Dyke"
       (URL "http://www.neilvandyke.org/htmlprag/"))
       )
     )

   (Description-unit "SSAX-pull"
     ("XML " (em "pull parsing") " and " (code "SSAX"))
     (body
       (p "SSAX parser can be considered a " (code "for-each") "-like
enumerator. The collection in question is the XML document itself (to
be more precise, the corresponding input port). Indeed, a sequential
reading of an XML document is equivalent to a depth-first traversal of
the corresponding XML Infoset tree. An article " (Cite "General ways
to traverse collections") " demonstrated that every " (code "for-each") "
enumerator can be turned inside-out. The result is a lazy list, a
demand-driven producer of values, which is called generator in
languages such as Icon, Python and Ruby. The user of the inverted SSAX
is no longer being pushed by the parser -- on contrary, the user
" (em "pulls") " markup and character data from the inverted-SSAX stream.
Thus SSAX can be turned into an XML Pull parser mechanically.")
       (p "We should note that many parts of the SSAX parser toolkit
have been designed with the pull model in mind. The function " (code
"lex-html") " of the permissive HTML parser illustrates how to pull
with SSAX.")  )
     (references
       (p (a (@ (href "enumerators-callcc.html"))
	    "General ways to traverse collections"))
      (URL "http://www.xmlpull.org/") (br)
	"XML Pull: Common API for XML Pull Parsing"
       (p (local-ref "HTML-parser"))
       (p
	 (a (@ (href "../continuations/differentiating-parsers.html"))
	   "Differentiating Parsers"))
      ))


(Description-unit "parent-ptr"
  "On parent pointers in SXML trees"
  (body
    (p
      "In this article we discuss and compare five different methods of
determining the parent of an SXML node. The existence of these methods
is a crucial step in a constructive proof that SXML is a complete
model of the XML Information set, and SXPath is a complete
implementation of the XPath Recommendation. Four of the five
techniques of determining the parenthood require no mutations, and can
be implemented in a pure and strict functional language. Three of the
five techniques rely on annotations attached to SXML nodes. The
techniques differ in the nature of the annotations and in the time
complexity of attaching and using these annotations.")
    (p
      "The source code shows several advanced examples of XML
SAX-style parsing: custom parsers that maintain node dictionaries and
parent links " (em "as") " they read the document.")
    )
  (version "1.1 Feb 12, 2003")
  (references
    (p (textref "parent-pointers.txt"
      "The article itself")
      )
    (p
      (fileref "parent-pointers.scm"
	"The complete source code of four techniques."))
    (p
      (a (@ (href
	      "http://sourceforge.net/mailarchive/forum.php?forum=ssax-sxml"))
	 "Discussion forum") (br)
      "Comments posted by Dmitry Lizorkin on Feb 22 and 27, 2003 are
particularly interesting. He has suggested a way of accounting for the
parent pointers during the evaluation of an (S)XPath expression.") ))


(Description-unit "validation"
  "SSAX parsing with limited XML doctype validation and datatype conversion"
  (body
    (p
      "XML Schema introduces a number of atomic XML datatypes and the
corresponding validation rules. SXML likewise supports boolean,
numerical, and other Scheme values [1] in the \"character\" content of
SXML elements and attributes. As we parse the document, we can
validate and de-serialize character data from the source XML
document into the corresponding numerical etc. Scheme values. The
present sample code shows an example of such datatype conversion and
validation. This code also demonstrates element content validation:
making sure that the sequence of child elements matches a
user-specified template.")
    (p
      "The validation in the present example is intentionally limited, to
keep the code simple and clear. The code can be extended to more
complex validation rules.")
    (p
      "[1] One may use any Scheme value in the \"character\" content
of an SXML element or attribute as long as this value is not a pair or
a symbol. If we need to insert a pair or a symbol in the \"character\"
content, we should wrap them in a vector or a procedure.")
    )
  (version "1.1 Aug 22, 2003")
  (references
    (fileref "validate-doctype-simple.scm"
      "The source code that instantiates the SSAX parser framework to
do on-the-fly document type validation and datatype conversion. The
tests at the end of the file demonstrate the datatype conversion and
the detection of invalid atomic and structural XML fragments."))  
)


;------------------------------------------------------------------------
   (p (hr))
   (h2 (@ (align "center"))  "Miscellanea")

   (Description-unit "PCDATA-ANY"
     ((code "CDATA") ", " (code "#PCDATA") ", and " (code "ANY"))
     (body
      "A short article explaining the difference among " (code "CDATA") 
      ", " (code "#PCDATA") " and " (code "ANY") " -- as well as the distinction between a mixed content and " (em "any") " content."
      )
     (version "1.1, Jan 5, 2001")
     (references
      (textref "PCDATA.txt"
	       "Re: XEXPR needs Schemers' help"
	       "An article posted on a comp.lang.scheme newsgroup on Fri, 05 Jan 2001 22:20:06 GMT.")))


   (Description-unit "SOAP-SXML"
     "SOAP 1.2 and SXML"
     (body
       (p
	 "SOAP 1.2 became a W3C Recommendation on June 24, 2003. SOAP
1.2, which used to be known as XML protocol, is quite different from
SOAP 1.1 and XML-RPC. In particular, SOAP 1.2 permits SXML (s-expression)
message systems to claim SOAP 1.2 compliance. Indeed,
SOAP does " (em "not") " require XML! The Recommendation says,")
       (blockquote
	 "A SOAP message is specified as an XML Information Set [XML
InfoSet]. While all SOAP message examples in this document are shown
using XML 1.0 syntax, other representations MAY be used to
transmit SOAP messages between nodes (see 4. SOAP Protocol Binding
Framework)." (br)
	 "..." (br)
	 "The binding framework does not require that every binding
use the XML 1.0 serialization as the \"on the wire\" representation of
the XML infoset; compressed, encrypted, fragmented representations and
so on can be used if appropriate.")
       (p
	 "In other words, the W3 Consortium specifically leaves it to
applications to choose the appropriate on-the-wire representation of
the XML Infoset. S-expressions is one of the realizations of the XML
Infoset. Thus serializing SOAP messages using SXML is specifically
permitted. Indeed, S-expressions may reasonably be characterized as a
\"compressed representation\" of the XML Infoset.")
      )
     (version "1.1, May 26, 2003")
     (references
      (textref "soap-sxml.txt"
	       "SOAP 1.2"
	       "A message with more discussion of the SOAP processing with
SXML. The message was posted on a SSAX-SXML mailing list on Mon, 26
May 2003 12:53:24 -0700.")))


   (Description-unit "eval-SXML"
     ((em "Evaluating SXML:") " XSL Transformations in Scheme")
     (body
      "Write a procedure " (code "xml") " such that an expression"
      (verbatim
       "(xml '((url \"mypage.html\" (name \"FirstName LastName's page\")) (p \"my paragraph\")))")
      "evaluates to a string"
      (verbatim
       "<a href=\"mypage.html\"><name>FirstName LastName's page</name></a><p>my paragraph</p>")
      (p "and")
      (verbatim
       "(define myname '(name \"FirstName LastName\"))"
       "(xml `( ((p \"par1\") (p \"par2\")) (p \"This is \" ,myname \" page\") ))")
      "evaluates to"
      (verbatim
       "<p>par1</p><p>par2</p><p>This is <name>FirstName LastName</name> page</p>")
      )
     (version "1.2, Jun 7, 2000")
     (references
      (p (textref "eval-Scheme-to-XML.txt"
		  "Evaluating Scheme to XML/HTML [Was: eval to invoke a local function?]"
		  "An article posted on a comp.lang.scheme newsgroup
on Thu, 27 Jul 2000 01:41:52 GMT. It contains the source code for the
" (code "xml") " function above, and discusses several generalizations."))
      (p (local-ref "SXSLT"))
      )
     )

   (Description-unit "executable-XML"
     "XEXPR, SXML, and \"executable\" XML code"
     (body
      (p "This set of two articles was prompted by a W3C technical
note that introduces XEXPR -- a programming (scripting) language with
XML notation. The second article offers a detailed critique. What if
we still want a programming language with an XML notation? The
articles consider two ways of designing such languages. One is to
assign an executable semantics, reduction rules, to an abstract syntax
tree representation of an XML document. The other approach is to start
with an extant programming language and to introduce rules to
pretty-print its abstract syntax tree into the XML format.")
      (p "Both approaches are especially easy if the programming
language is Scheme. The concrete syntax of Scheme is basically its
abstract syntax as well. On the other hand, SXML -- an abstract syntax
for XML -- has a representation that parses not only as Scheme
\"data\" but also as a Scheme \"expression\" (by R5RS syntax
definitions). In SXML, abstract syntaxes of Scheme and XML unify.")
      (p "The two approaches are illustrated in the articles below. In
particular, the second article demonstrates that a SXML pretty-printer
is general enough to permit a well-defined XML syntax for Scheme. This
is not trivial as the following Scheme expression may show:")
      (verbatim
    "(begin (append a (x y) '(b) '(c \"d\" 1 (f g) '(h i) `g () '() (()) j)))"
       )
      "It exhibits a great deal of context-sensitivity.  What looks like an
application may in reality be something else, if it is preceded by a
quote. A quote within a quote loses its special meaning. An XML
syntax should be able to capture these nuances -- and it does as the 
articles show."
      (p "To see how we can assign an XML notation to Scheme, or an evaluation
semantics to XML, let us consider an expression:")
      (verbatim
       "(string-join '(\"foo\" \"bar\" \"baz\") \":\")"
       )
      "The code in the first article pretty-prints this expression as"
      (verbatim
       "<string-join>"
       "  <list> <str>foo</str><str>bar</str><str>baz</str> </list>"
       "  <str>:</str>"
       "</string-join>"
       )
      "On the other hand, we start with this XML string and feed it
into a SSAX parser. We will immediately see that the output:"
      (verbatim
       "(string-join (list (str \"foo\") (str \"bar\") (str \"baz\")) (str \":\"))"
       )
      "can be assigned a well-defined evaluation semantics -- the
semantics of Scheme. Indeed, this is a genuine Scheme expression, and
can be evaluated as such. The result will be the same as that of the
original expression, provided that we define " (code "str") " as the
identity function."
      (p "The second article discusses another, more general XML notation for
our example:")
      (verbatim
       "<apply>"
       "  <var>string-join</var>"
       "     <list> <str>foo</str><str>bar</str><str>baz</str> </list>"
       "     <str>:</str>"
       "</apply>")
      "This representation holds a stronger invariant with respect to
SXML pretty-printing and parsing."
      (p
       "The first article notes that a function " (code "post-order")
" at the heart of SXML pretty-printing can also evaluate a Scheme
expression directly. Indeed, this function " (em "is") " an
implementation of " (code "eval") " -- or " (code "eval") " is a
post-order traversal for a Scheme expression tree.")
      )
     (version "1.1, Jan 17, 2001")
     (references
      (p (textref "XML-as-Scheme-1.txt"
		  "XML as Scheme, or Scheme as XML"
		  "The first article, posted on comp.lang.scheme and comp.text.xml newsgroups on Sat Jan 06 02:45:03 2001 GMT."))
      (p (textref "XML-as-Scheme-2.txt"
		  "XML as Scheme, Scheme as XML; XEXPR considered invalid"
		  "The second article, posted on comp.lang.scheme and comp.text.xml newsgroups on Wed Jan 17 06:58:14 2001 GMT."))
      (p (a (@ (href "http://www.w3.org/TR/2000/NOTE-xexpr-20001121"))
	    "XEXPR - A Scripting Language for XML") ". Gavin Thomas Nicol, Ed. W3C Note 21 November 2000.")
      (p (a (@
   (href "http://www.cs.bell-labs.com/who/wadler/papers/next700/next700.pdf"))
	    "The Next 700 Markup Languages")
	 " by Philip Wadler." (br)
	 "Invited Talk, Second Conference on Domain Specific Languages (DSL'99), Austin, Texas, October 1999." (br)
"Among other things, this talk gives an interesting example of a
functional programming language with an XML syntax.")
      (p (a (@
     (href "http://groups.google.com/groups?as_q=XEXPR&as_ugroup=comp.lang.scheme&as_drrb=b&as_mind=1&as_minm=1&as_miny=2001&as_maxd=19&as_maxm=1&as_maxy=2001")) "XEXPR and Scheme discussion thread"))
     )
     (requires
      (
       (local-ref "XML-authoring"
		 "SXML expression tree transformers")
       )
      )
)
   (footer)

)))

;(pp Content)

;========================================================================
;			HTML generation

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports


; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content
   (generic-web-rules Content '()))))

(generate-HTML Content)
