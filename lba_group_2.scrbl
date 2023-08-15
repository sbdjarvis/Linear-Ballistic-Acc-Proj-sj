#lang scribble/base
@(require scribble/base
          scribble/manual
          scriblib/figure
          scriblib/autobib
          scribble-math/dollar)

@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title{Linear Ballistic Accumulators: Defining the simplest complete model of choice response time}
@author{Baaba, Tee and Simon}

Decision-making is an essential human skill that requires that a choice is made after available alternatives have been evaluated. This comprises the decision to stop ruminating over available options as well as the decision act itself.
Decision timeliness is important; it determines whether or not a decision is acceptable. However, as more information is accumulated, the quality of a decision increases.
The choice under consideration may be an elementary, binary choice problem, with two possible response options. Other decision-making paradigms may require choosing between multiple alternatives, in which case decision-makers apply compensatory
and non-compensatory decision-making strategies, screening and eliminating possible alternatives and making trade-offs. When compensatory decision-making strategies are used, positive and negative qualities of all possible alternatives are evaluated such that positive qualities compensate for negative ones (@~cite[plt-tr6].
In non-compensatory decision-making, alternatives are eliminated based on how well they meet a given criterion.

@section{What is an accumulator model?}
An accumulator model – also known as a sequential sampling model – is a computational model of decision-making that simulates the decision-making process in various experiments @~cite[plt-tr1] @~cite[plt-tr2]). Accumulator models explain that decisions are implemented through the accumulation of noisy evidence.
A response is selected once enough evidence has been collected and a response threshold is reached. The accumulator model is associated with a number of parameters – an encoding parameter, a drift rate parameter which represents the average rate of evidence accumulation, a starting point parameter which represents the initial state of the accumulator,
a threshold parameter and a motor response time parameter which defines when a response should be executed. @~cite[plt-tr4] @~cite[plt-tr3].

@section{What exactly is a Linear Ballistic Accumulator and why is it a good model?}
The Linear Ballistic Accumulator Model is a model of choice response time that is simplified compared to other models. It can be used to model a choice between two alternatives but is preferred because it also models a choice between multiple alternatives. The Linear Ballistic Accumulator eliminates the moment-to-moment variability that earlier models like the
Usher and McClelland Model incorporate. Evidence is accumulated for each response independent of other responses, which ensures that accurate predictions for both correct and incorrect responses are made. It also provides analytic solutions for the predicted distributions and probabilities which are easy to use @~cite[plt-tr2].

@figure**[
        "fig:lbamodelled"
        @elem{Linear Ballistic Accumulator Described}
        @elem{@image[#:scale 0.3]{./lbamodelled.png}}]

@subsection{Applications of LBA} 
Linear Ballistic Accumulators are mostly used to model the decision-making processes in tasks that involve choosing between multiple alternatives. As well,
they are frequently used in experiments that measure reaction time in order to analyze response times and accuracy.
It may be useful for predictive modelling as it allows researchers to make predictions about response times and accuracy in binary and multiple decision tasks based on model parameters.
As well, it provides insights into cognitive processes that underlie decision-making, such as the speed of evidence accumulation @~cite[plt-tr2]. 
Specific experiments that the authors had applied the linear ballistic accumulator to are the lexical decision task and speed and accuracy emphasis in brightness distinctions where the lexical decision task is a binary task while
the speed and accuracy emphasis in brightness discrimination is a multiple-decision task in which the participant must choose from n>2 choices @~cite[plt-tr2].

@subsubsection{Lexical Decision Task} 
The Lexical Decision Task is a task in which participants must classify words as either valid words or non-words. For example, a valid word would be "LOAF" and a non-word would be "LORG".).
In the paper, stimulus features such as the ease of pronunciation for non-words and the natural occurrence frequency for words were manipulated. They found reliable changes in response accuracy, and the shapes of RT distributions for correct and incorrect responses @~cite[plt-tr2].

So how did the LBA perform compared to the data of the participants?

@figure**[
        "fig:lexdecisiontask"
        @elem{Lexical Decision task data demonstrated with the LBA model closely following the data}
        @elem{@image[#:scale 1.0]{./lexdecisiontask.png}}]

The three graphs represent data from three stimulus classes: pseudowords, high-frequency words and low-frequency words. The two lines show .1-, .3-, .5-, .7-, .9- quantile estimates for correct responses
which are the upper lines and incorrect responses which are the lower lines @~cite[plt-tr2]. 

The model's predictions were able to match this data very well as the worst error is a 0.053-s over-prediction for the 90% quantile estimate from incorrect responses to low frequency words. 
Ratcliff modeled the data with their diffusion model which required k+6 free parameters when applied to k different experimental conditions. LBA accounts from the same data at least as well using k+4 parameters
As the model is simplified, it uses fewer parameters but it got relatively accurate results which implies that this model, although it isn't as complicated, can still accomplish what it is meant to do.
The LBA parameters reflect consistent underlying aspects of decision-making and it shows that the LBA accounts for response accuracy and for the shape and speed of RT (Reaction Time) distribution for both correct AND incorrect choices @~cite[plt-tr2]. 

@subsubsection{Speed and accuracy emphasis in brightness discrimination} 
Brown & Heathcote also modelled a decision-making experiment performed by Ratcliffe & Rouder. In this experiment, Ratcliffe & Rouder asked three participants to classify patches of pixels as either “bright” or “dark” about 8000 times each. They manipulated the brightness of the patch in 33 levels from very dark, through to midway to very bright on a scale. They also manipulated the urgency of which the participants should respond.
Participants were given two conditions: speed-emphasis instructions in which they were told to prioritize their speed rather than accuracy and then accuracy-emphasis instructions in which they were told not to make too many mistakes. 

@figure**[
        "fig:speedaccuracytask"
        @elem{Speed and accuracy task with the LBA performance}
        @elem{@image[#:scale 1.0]{./speedaccuracypic.png}}]

The upper and lower lines are for accuracy and speed emphasis conditions, respectively.  Following Ratcliff and Rouder’s analyses, the authors also collapsed “bright” and “dark” responses, so that the left side of each plot represented correct responses to very easy-to-classify stimuli, and the right side 
of each plot represents (very rare) incorrect responses to the same stimuli. The center of each plot shows data from difficult stimuli, which were nearly equally often classified as “dark” or “bright”. The LBA’s performance is the solid black line in each model @~cite[plt-tr2]. 

@section{LBA Replication Experiment}

In order to develop our understanding of the LBA model and its applications, we attempted to replicate Brown & Heathcote's lexical decision task model @~cite[plt-tr2].
While this is only a simple binary decision model and does not demonstrate the versatility of a linear ballistic accumulator
has over other accumulator models, it does have the virtue of ease of understanding through its simplicity.

The equations governing an LBA are shown in equations (1), (2), and (3).


@(use-mathjax)

@elemtag{foft}
@[$$ "f_i(t) = \\frac{1}{A} \\big[-v_i Φ\\Big( \\frac{b-A-tv_i}{ts} \\Big) + sϕ\\Big( \\frac{b-A-tv_i}{ts} \\Big) + v_i Φ \\Big( \\frac{b-tv_i}{ts} \\Big) -sϕ \\Big( \\frac{b-tv_i}{ts} \\Big) \\Big]"]{\tag{1}}

@elemtag{Foft}
@[$$ "F_i(t) = 1 + \\frac{b-A-tv_i}{A} Φ\\Big( \\frac{b-A-tv_i}{ts}\\Big)-\\frac{b-tv_i}{A}Φ\\Big(\\frac{b-tv_i}{ts}\\Big) + \\frac{ts}{A}ϕ \\Big( \\frac{b-A-tv_i}{ts}\\Big) - \\frac{ts}{A}ϕ\\Big( \\frac{b-tv_i}{ts}\\Big)"]{\tag{2}}

@elemtag{PDF}
@[$$ "PDF_i ~=~ f(t)_i \\prod_{i \\neq j}(1-F_j(t))"]{\tag{3}}

@margin-note{b, A, and v are the parameters which define the model:

b: the threshold at which a decision is made

A: maximum range for evidence starting value

v: average slope of drift rate
             }

Equation (1) represents the probability distribution function of an individual
accumulator reaching a decision at a given time, while (2) gives an accumulator’s cumulative distribution function. Combining (1) & (2) in equation (3) gives the defective
probability for an accumulator, whose numerical integral gives us the defective cumulative probability function (CDF), which Brown & Heathcote used to generate their
final output modelling Ratcliff et al.’s data. @~cite[plt-tr2] @~cite[plt-tr8]

We translated (3) into Racket code (please see ), and, using a numerical integration package @~cite[plt-tr7] obtained the following results:

@figure**[
        "fig:CDFshortplots"
        @elem{Defective cumulative distribution curves for Brown & Heathcote's (2008) model of Ratcliff et al.'s (2004) experiment 3. vP, vH, and vL refer to Pseudowords, High Frequency words, and Low Frequency words, respectively. "Correct" and "Incorrect" curves refer to the likelihood of an
              accumulator's reaching a decision at the given time.}
        @elem{@image[#:scale 0.4]{./CDFshortPlot.png}}]


Clearly, some mathematical mistranslation has occurred, though its precise nature eludes our abilities.
When plotted over a less realistic time frame, the CDF plots begin to show their nonlinearity, but also become less reasonable:

@figure**[
        "fig:CDFlongplots"
        @elem{Defective cumulative distribution curves for Brown & Heathcote's (2008) model of Ratcliff et al.'s (2004) experiment 3 plotted over a 100s timeframe.
  Note that the cumulative probability decreases over time, which is clearly incorrect.}
        @elem{@image[#:scale 0.4]{./CDFlongPlot.png}}]

As seen in Figure 6, plotting Equation (2) over the same time intervals produces more comprehensible results.

@figure**[
        "fig:F(t)plots"
        @elem{Non-defective cumulative distribution curves for Brown & Heathcote's (2008) model of Ratcliff et al.'s (2004) experiment 3. Compare curve shapes with those in Figure 7.}
        @elem{@image[#:scale 0.4]{./FtPlot.png}}]
@figure**[
        "fig:BandHplots"
        @elem{From Brown & Heathcote (2008). Open symbols represent Ratcliff et al.'s (2004) experimental data, solid lines represent Brown & Heathcote's (2008) modelled data.}
        @elem{@image[#:scale 0.8]{./BHplots.png}}]

The curves much more closely match the shape of the data published by Ratcliff et al. (2004),
though still not a correct prediction. As a proof-of-concept, however, this was at least
 valid, requiring only better mathematical understanding of the problem at hand to perfect it. Ultimately, while unsuccessful as an exact replication, this 
exercise was a useful exploration of the underlying principles of the LBA as a decision-making model.

                           
@section{Limitations of the Model}
As the LBA is a simplification of Usher and Clelland's (2001) leaky competitive accumulator, the leaky competitive accumulator has received support from arguments regarding the evidence of neural plausibility.
However, the LBA may be criticized as it omits the within-trial variability (a basic feature of neural activity) rendering it "neurally implausible" @~cite[plt-tr2].

The authors outline 2 defences against that criticism.

@subsection{Approximation of physiological phenomenon} 
All models are approximations and include simplifications for convenience. All the models of choice RT approximate many things between neurophysiological reality and theoretical description
such as ion flows, membrane potentials and action potentials. The LBA model adds another further step but the approximations shouldn't be seen as harmful towards scientific progress.
The simplicity of the LBA model might allow it to be applied to a wider range of choice RT paradigms that might not have been possible before @~cite[plt-tr2]. 

@subsection{Neurophysiological literature versus psychological literature and neural plausability}
There are many leading models of choice phenomena from neurophysiological literature that omit within-trial variability as the LBA does. One example can be Reddi and Carpenter's (2000) LATER model
which only has one response accumulator and does not consider between-trial variability in the starting points of evidence accumulation. This model is similar to LBA however its limitations
mean that the LATER model provides an incomplete account of behaviour data, specifically of error responses. The point is that if the neurophysiological community can accept models that omit within-trial variability,
the psychological community should be careful about ruling out the LBA on the grounds of "neural plausibility" @~cite[plt-tr2].

@section{Concluding thoughts and questions}
The concluding questions and thoughts we had were related to the criticism of the LBA. We wonder if perhaps it has been oversimplified and skimps on the applications of physiological processes. What we mean by this is that
perhaps it is too rigid because of its simplifications. It doesn't take into account some processes that actually occur in the brain such as delays in decision-making times and well as competing decisions. For example, if you were taking
a multiple choice test and in your mind, you know that the answer is B but the decision to make the answer C overrides that first decision. The LBA does not consider that natural process and it brings the question of whether it is too oversimplified since indecisiveness
or dwelling on decisions are big phenomena that every person experiences. Decisions are inherently dynamic and adaptable; they are not fixed. Each choice necessitates a process of assessing trade-offs and involves the deliberation of the decision maker in assigning relative significance to individual attributes @~cite[plt-tr6].
The simplicity of the LBA model makes it preferable to other choice models, as it is both easier to grasp conceptually and easier to implement in practice

@(define plt-tr1
   (make-bib
    #:title     "A ballistic model of choice response time"
    #:author    (authors "Brown, S. D." "Heathcote, A.")
    #:date      "2005"
    #:location  (journal-location "Psychological review" #:pages '(117 128) #:volume "112")
    #:note      @hyperlink["https://doi.org/10.1037/0033-295x.112.1.117"]{https://doi.org/10.1037/0033-295x.112.1.117} ))

@(define plt-tr2
   (make-bib
    #:title     "The simplest complete model of choice response time: linear ballistic accumulation"
    #:author    (authors "Brown, S. D." "Heathcote, A.")
    #:date      "2008"
    #:location  (journal-location "Cognitive Psychology" #:pages '(153 178) #:number "3" #:volume "57")
    #:note      @hyperlink["https://doi.org/10.1016/j.cogpsych.2007.12.002"]{https://doi.org/10.1016/j.cogpsych.2007.12.002} ))
    
@(define plt-tr3
   (make-bib
    #:title     "A parallel accumulator model accounts for decision randomness when deciding on risky prospects with different expected value"
    #:author    (authors "Howlett, J. R." "Paulus, M. P.")
    #:date      "2020"
    #:location  (journal-location "PloS one" #:number "7" #:volume "15")
    #:note       @hyperlink["https://doi.org/10.1371/journal.pone.0233761"]{https://doi.org/10.1371/journal.pone.0233761} ))
    
@(define plt-tr4
   (make-bib
    #:title     "RELATING ACCUMULATOR MODEL PARAMETERS AND NEURAL DYNAMICS"
    #:author    (authors "Purcell, B. A." "Palmeri, T. J.")
    #:date      "2017"
    #:location  (journal-location "Journal of mathematical psychology" #:pages '(156 171) #:number "B" #:volume "76")
    #:note      @hyperlink["https://doi.org/10.1016/j.jmp.2016.07.001"]{https://doi.org/10.1016/j.jmp.2016.07.001} ))

@(define plt-tr5
   (make-bib
    #:title     "Modeling response times for two-choice decisions"
    #:author    (authors "Ratcliff, R." "Rouder, J.N.")
    #:date      "1998"
    #:location  (journal-location "Psychological Science" #:pages '(347 356) #:number "5" #:volume "9")
    #:note      @hyperlink["https://doi.org/10.1111/1467-9280.00067"]{https://doi.org/10.1111/1467-9280.00067} ))

@(define plt-tr6
   (make-bib
    #:title     "Decision-Making Processes in Social Contexts"
    #:author    (authors "Bruch, E." "Feinberg, F. F.")
    #:date      "2017"
    #:location  (journal-location "Annual Review of Sociology" #:pages '(207 227)  #:volume "43")
    #:note      @hyperlink["https://doi.org/10.1146/annurev-soc-060116-053622"]{https://doi.org/10.1146/annurev-soc-060116-053622} ))

@(define plt-tr7
   (make-bib
    #:title     "Racket_NumericalMethods: A collection of Numerical Methods implemented in Racket"
    #:author    (authors "Kierzenka, M.")
    #:location  (journal-location "Github")
    #:note      @hyperlink["https://github.com/mkierzenka/Racket_NumericalMethods"]{https://github.com/mkierzenka/Racket_NumericalMethods} )) 

@(define plt-tr8
   (make-bib
    #:title     "A comparison of sequential sampling models for two-choice reaction time"
    #:author    (authors "Ratcliff, R." "Smith, P.L.")
    #:date      "2004"
    #:location  (journal-location "Psychological Review" #:pages '(333 367) #:volume "111")
    #:note      @hyperlink["https://psycnet.apa.org/doi/10.1037/0033-295X.111.2.333"]{https://psycnet.apa.org/doi/10.1037/0033-295X.111.2.333} ))
    
@generate-bibliography[#:sec-title "Project References"
                         #:tag "ref:projects"]

@hyperlink["./Appendix1.html"]{Appendix 1}
