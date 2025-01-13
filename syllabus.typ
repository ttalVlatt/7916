// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}
#show: doc => article(
  paper: "us-letter",
  font: ("Arial",),
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#show heading.where(level: 1): set align(center)
#show heading.where(level: 2): set align(center)
#set page(margin: 0.5in)
#show link: underline
#set table(fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") }, stroke: 0.25pt)
#set par(justify: false)
#set list(tight: true)

#align(center)[
#image("site-attachments/UF-CoE-Logo.png", height: 0.5in)
= EDH 7916: Contemporary Research in Higher Education
<edh-7916-contemporary-research-in-higher-education>

#v(0.1in)
#h(1fr) #text(size: 16pt)[Spring 2025] #h(1fr)

#v(0.1in)

*Class Meeting Time* \
Wednesday 5:10pm - 8:10pm \
*Class Location* \
NRN 2025


#grid(columns: 2,
      alignment: center,
      gutter: 0.5in,


[ Jue Wu, Ph.D. \
  Email: #link("mailto:juewu@coe.ufl.edu")[juewu\@coe.ufl.edu] \
  Office Hours: Thursday 10am - 12pm \
  Office Location: Norman Hall 2705-B],
[Matt Capaldi \
  Email: #link("mailto:m.capaldi@ufl.edu")[m.capaldi\@ufl.edu] \
  Office Hours: Monday 2pm - 4pm \
  Office Location: Norman Hall 2705-P]
)
]
== Course Description
<course-description>
Contemporary higher education researchers have a wide variety of quantitative tools at their disposal. Yet as the number and sophistication of these tools grows, so too do expectations about the quality of final analyses. Furthermore, increasing scrutiny of non-replicable results demands that researchers follow a proper workflow to mitigate errors. In this course, students will learn the fundamentals of a quantitative research workflow and apply these lessons using common open-source tools. We will cover key skills for crafting reports & publications including project organization, data wrangling/cleaning, and data visualization. Throughout, students will use coding best-practices so that their workflow may be shared and easily reproduced.

== Course Objectives
<course-objectives>
Students will learn

+ Basic data management principles & skills needed for contemporary research in higher education
+ How to create reproducible research as increasingly required in contemporary higher education research
+ A foundational knowledge of the R, R Studio, & Quarto

== Texts
<texts>
=== Required
<required>
- No required textbook
- Course website #link("https://capaldi.info/7916")[capaldi.info/7916] contains all required content

=== Recommended
<recommended>
- There are numerous online resources to help learn R, one of the most comprehensive being #link("https://r4ds.hadley.nz")[R for data science];, which is available online for free

== Required tools, software, and registrations
<required-tools-software-and-registrations>
Students will be expected to bring a laptop to class. It does not matter whether the machine runs MacOS, Windows, or Linux; however, the student’s machine needs to be relatively up to date and in good running order. It needs to be able to connect to the internet during class. All software is freely available to UF students.

=== Required
<required-1>
- #strong[R] #link("https://cran.r-project.org")[cran.r-project.org]
  - NOTE: if you have installed R on your machine in the past, make sure that you have the most up-to-date version \(new versions are released about once a quarter)
  - You will also be required to install a number of R packages throughout the course
- #strong[RStudio] #link("https://posit.co/download/rstudio-desktop/")[posit.co/download/rstudio-desktop/]
  - NOTE: if you have installed RStudio on your machine in the past, make sure that you have the most up-to-date version \(new versions are released about once a quarter)

=== Optional
<optional>
- #strong[git] \(version control software) #link("https://git-scm.com")[git-scm.com] & #strong[GitHub] \(git-specific cloud storage) #link("https://github.com")[github.com]
  - Using git to version-control your project is optional for extra-credit on your final report \(see extra credit assignment)
  - Students can sign up for a free GitHub account if they haven’t already : #link("https://github.com/join")[github.com/join]
  - Additionally, students \(using their university email address) can request a free GitHub pro account #link("https://education.github.com/benefits")[education.github.com/benefits] which enables access to #strong[GitHub Co-Pilot] add-in to RStudio

== Schedule
<schedule>
#figure(
align(center)[#table(
  columns: 4,
  align: (col, row) => (auto,auto,auto,auto,).at(col),
  inset: 6pt,
  [#strong[Week];], [#strong[Date];], [#strong[Lesson\(s)];], [#strong[Due Next Tuesday 12:00pm];],
  [1],
  [Jan-15],
  [- Welcome

  - Syllabus

  - Installing R & R Studio

  ],
  [None],
  [2],
  [Jan-22],
  [- Reading Data Into R

  - Intro to IPEDS

  - Intro to Copilot

  ],
  [- Assignment 1

  ],
  [3],
  [Jan-29],
  [- Data Wrangling I

  ],
  [- Assignment 2

  ],
  [4],
  [Feb-5],
  [- Data Wrangling II

  ],
  [- Assignment 3

  ],
  [5],
  [Feb-12],
  [- Data Visualization I

  ],
  [- Assignment 4

  - #strong[Reproducible Report];: Proposal

  ],
  [6],
  [Feb-19],
  [- Data Visualization II

  ],
  [- Assignment 5 \(Graph replication challenge)

  ],
  [7],
  [Feb-26],
  [- Reproducible Reports with Quarto

  ],
  [- Assignment 6

  ],
  [8],
  [Mar-5],
  [- Data Wrangling III

  ],
  [- Assignment 7

  ],
  [9],
  [Mar-12],
  [- Functional Programming

  ],
  [- Assignment 8

  ],
  [10],
  [Mar-19],
  [#strong[Spring Break, No Class Meeting];],
  [None, enjoy the break!],
  [11],
  [Mar-26],
  [- Data Wrangling IV

  ],
  [- #strong[Reproducible Report];: Initial Analysis

  ],
  [12],
  [Apr-2],
  [- Modeling Basics

  ],
  [- Assignment 9

  - #strong[Reproducible Report];: Draft \(Optional)

  ],
  [13],
  [Apr-9],
  [- Data Visualization III

  ],
  [- Assignment 10

  ],
  [14],
  [Apr-16],
  [- Reproducible Report: Presentations

  ],
  [- #strong[Reproducible Report];: Presentation

  ],
  [15],
  [Apr-23],
  [- Course Summary

  - Reproducible Report: Lab Time

  ],
  [- #strong[Reproducible Report];: Final Report

  - Extra-Credit Assignments \(Optional)

  ],
  [16],
  [Apr-30],
  [#strong[Finals Week, No Class Meeting];],
  [],
)]
, caption: [Class Schedule]
)

== Grading
<grading>
There are a total of 100 points for the class, plus extra credit opportunities.

=== Lesson Assignments \(50 points)
<lesson-assignments-50-points>
There are 10 lesson assignments \(see assignment tab on each lesson for details and Canvas for due dates), each worth 5 points. Assignments are always due by #strong[Tuesday 12pm following the lesson.]

You will receive one of the following grades

- #strong[5/5] – everything is correct
- #strong[4.5/5] – mostly correct, concepts are all understood, but slight error\(s)
- #strong[2.5/5] – at least one significant error, #strong[please re-submit]
  - You will have the chance to revise and resubmit the following week with corrections to get #strong[4/5]
- #strong[0/5] – not turned in on time \(unless excused)
  - You will have the chance to submit the following week for #strong[2.5/5]

If you are struggling and haven’t been able to complete the assignment, it is far better to turn in an incomplete assignment and get 2.5/5 with a chance to improve to 4/5 than miss the deadline

This grading system is designed to encourage you to revisit concepts that didn’t click the first time, not to be punitive. There are opportunities for extra credit to make up for lost points.

=== Class Participation and Quiz \(10 points)
<class-participation-and-quiz-10-points>
We will use class time to work through lesson modules together. Students are expected to follow along with the presentation and run code on their own machine. Students are also expected to answer questions, participate in discussions, and work through example problems throughout the class session.

There will be 5 quizzes throughout the semester, each worth 2 points. These quizzes will serve as attendance checks and assess your knowledge of the course content.

=== Reproducible Report \(40 points)
<reproducible-report-40-points>
Your final project grade comes from four elements, see the final project section of the class website for details.

- Proposal: 5 points
- Initial analysis: 10 points
- Presentation: 5 points
- Report: 20 points
  - Optional: You can submit a draft of the report for a preliminary grade and feedback on how to improve \(see schedule and/or Canvas for due date)

=== Extra-Credit Assignments
<extra-credit-assignments>
On the class website you will see lessons titled "Extra: …" which are opportunities for extra credit.

- Lesson follow the same structure as class lessons, review the lesson content then complete tasks in the assignment tab
- Time permitting, some extra credit lessons may be completed in class, assignments will still count for extra credit
- Each extra credit assignment is worth 2.5 points
- Simply make a good faith effort to complete the assignment and you will receive the points
- All extra credit assignments are due during week 15 \(see schedule and/or Canvas for due date)

#align(center)[
=== Grading Scale
<grading-scale>
#figure(
align(center)[#table(
  columns: 2,
  align: (col, row) => (auto,auto,).at(col),
  inset: 6pt,
  [Grade], [Score],
  [A],
  [93 or Above],
  [A-],
  [90 to 92.5],
  [B+],
  [87.5 to 89.5],
  [B],
  [83 to 87],
  [B-],
  [80 to 82.5],
  [C+],
  [77.5 to 79.5],
  [C],
  [73 to 77],
  [C-],
  [70 to 72.5],
  [D+],
  [67.5 to 69.5],
  [D],
  [63 to 67],
  [D-],
  [60 to 62.5],
  [E],
  [Below 60],
)]
, caption: [Grading Scale]
)

]
== Getting Coding Help
<getting-coding-help>
#block[
#set enum(numbering: "1)", start: 1)
+ Take a break, go outside, get some food
+ Talk to your #link("https://en.wikipedia.org/wiki/Rubber_duck_debugging")[rubber duck]
+ Talk to your classmates
  - Please acknowledge with a `## h/t`
+ Try Google or Stack Overflow
  - Please acknowledge with a `## h/t` \(it helps you later too!)
  - Caution: the internet does strange things to people… Sometimes people offering "help" can be unnecessarily blunt and/or mean, particularly to people just starting out
+ If you use Github Copilot to help you with your code
  - Please keep your prompts and acknowledge with a `## h/t`
+ Office hours or email
]

== UF Graduate School Policies
<uf-graduate-school-policies>
See #link("https://gradcatalog.ufl.edu/graduate/regulations/")[UF Graduate School policies] on grading, attendance, academic integrity, and more.

== Honor code
<honor-code>
UF students are bound by The Honor Pledge which states,

#quote(block: true)[
We, the members of the University of Florida community, pledge to hold ourselves and our peers to the highest standards of honor and integrity by abiding by the Honor Code. On all work submitted for credit by students at the University of Florida, the following pledge is either required or implied: "On my honor, I have neither given nor received unauthorized aid in doing this assignment."
]

The #link("https://sccr.dso.ufl.edu/policies/student-honor-code-student-conduct-code/")[Honor Code] specifies a number of behaviors that are in violation of this code and the possible sanctions. Furthermore, you are obligated to report any condition that facilitates academic misconduct to appropriate personnel. If you have any questions or concerns, please consult with the instructor or TAs in this class.

== Accommodations
<accommodations>
Students with disabilities who experience learning barriers and would like to request academic accommodations should #link("https://disability.ufl.edu/students/get-started/")[connect with the disability Resource Center];. It is important for students to share their accommodation letter with their instructor and discuss their access needs, as early as possible in the semester.

== Course evaluations
<course-evaluations>
Students are expected to provide professional and respectful feedback on the quality of instruction in this course by completing course evaluations online via GatorEvals. Guidance on how to give feedback in a professional and respectful manner is available #link("https://gatorevals.aa.ufl.edu/students/")[here];. Students will be notified when the evaluation period opens, and can complete evaluations through the email they receive from GatorEvals, in their Canvas course menu under GatorEvals, or #link("https://ufl.bluera.com/ufl/")[here];. Summaries of course evaluation results are available to students #link("https://gatorevals.aa.ufl.edu/public-results/")[here];.

== In-class recording
<in-class-recording>
Students are allowed to record video or audio of class lectures. However, the purposes for which these recordings may be used are strictly controlled. The only allowable purposes are \(1) for personal educational use, \(2) in connection with a complaint to the university, or \(3) as evidence in, or in preparation for, a criminal or civil proceeding. All other purposes are prohibited. Specifically, students may not publish recorded lectures without the written consent of the instructor.

A "class lecture" is an educational presentation intended to inform or teach enrolled students about a particular subject, including any instructor-led discussions that form part of the presentation, and delivered by any instructor hired or appointed by the University, or by a guest instructor, as part of a University of Florida course. A class lecture does not include lab sessions, student presentations, clinical presentations such as patient history, academic exercises involving solely student participation, assessments \(quizzes, tests, exams), field trips, private conversations between students in the class or between a student and the faculty or lecturer during a class session.

Publication without permission of the instructor is prohibited. To "publish" means to share, transmit, circulate, distribute, or provide access to a recording, regardless of format or medium, to an- other person \(or persons), including but not limited to another student within the same class section. Additionally, a recording, or transcript of a recording, is considered published if it is posted on or uploaded to, in whole or in part, any media platform, including but not limited to social media, book, magazine, newspaper, leaflet, or third party note/tutoring services. A student who publishes a recording without written consent may be subject to a civil cause of action instituted by a person injured by the publication and/or discipline under UF Regulation 4.040 Student Honor Code and Student Conduct Code.
