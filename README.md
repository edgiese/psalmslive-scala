# psalmslive-scala
These are scala-based resources for my "Psalms Live" project.  The purpose of Psalms Live is to provide a means for anyone interested to read a portion of the psalms every day, on a schedule that traverses the entire Psalter four times per year.  Under this wrapper project, which started as something of a hobby and something for myself, I've tried to experiment with slightly different ways of doing things.

This repo is intended to eventually hold a number of experimental projects, which may grow into something more if they take off.  My intention from the beginning is to share whatever I do with the world on this project, if anyone is interested.

## Psalm Structure
One important piece of this project is an internal representation of psalms.  Psalms are part of Scripture, but are simpler than Scripture because of several simplifying assumptions:

1. Every translation I know begins new paragraphs at new "verse numbers" with the Psalms.  In Scripture generally, chapter and verse markings are arbitrary constructs of later scribes, who didn't always choose appropriate breaking points. It is not unknown for a modern translation to incorporate a chapter break inside of a paragraph, which presents typographical challenges to someone wishing to represent that translation well.  Not so with the Psalms; each "chapter" is always treated as a separate Psalm altogether.  Although there are cases where two psalms are thought to be together originally (such as Pss. 9 and 10), no modern translation attempts to show them as a unified poem.
2. All of the psalms are shown as poetry in modern translations, in short lines with just two levels of indentation.
3. All typographic representation in the psalms can be reduced to a few simple rules, such as the representation of the Tetragrammaton (the Hebrew name for God) as "Lord" in small caps.
4. No citations of psalms that I know of break verses in the middle of lines.

Given these simplifications, a psalm can be representated as follows:

1. The psalm number
2. An optional introduction (note:  this was "verse 1" in the Masoretic rendering, but all other translations begin verse 1 with the actual poetry, which is what my representation does.)  The introductory text may have footnotes, or use the Tetragrammaton, but has no other typographical oddities.
3. An array of lines divided into verses.  Each line is either:
  1. a blank line
  2. a line of text with indentation.  The text has no typographical oddities except the Tetragrammaton, or a footnote anchor.
  3. a subheading, used only in Ps. 119 for the letters of the Hebrew alphabet.
4. An array of footnotes.  These must have a one-to-one correspondence with the anchors in the verse lines above.  Inside the footnotes, many translations italicize alternate readings, although quotations are what my format uses.

I represented this psalm structure in scala using a class.  One nice feature of Scala allowed me quite easily to implement a very nice toString method, which produces output such as this example, Psalm 3 ESV:

````
Psalm 3
A Psalm of David, when he fled from Absalom his son.
  1  O LORD, how many are my foes!
       Many are rising against me;
  2  many are saying of my soul,
       “There is no salvation for him in God.” Selah*
     
  3  But you, O LORD, are a shield about me,
       my glory, and the lifter of my head.
  4  I cried aloud to the LORD,
       and he answered me from his holy hill. Selah
     
  5  I lay down and slept;
       I woke again, for the LORD sustained me.
  6  I will not be afraid of many thousands of people
       who have set themselves against me all around.
     
  7  Arise, O LORD!
       Save me, O my God!
     For you strike all my enemies on the cheek;
       you break the teeth of the wicked.
     
  8  Salvation belongs to the LORD;
       your blessing be on your people! Selah
     

Footnotes:
The meaning of the Hebrew word "Selah", used frequently in the Psalms, is uncertain. It may be a musical or liturgical direction
````
