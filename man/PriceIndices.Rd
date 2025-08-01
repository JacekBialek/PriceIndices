% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PriceIndices.R
\docType{package}
\name{PriceIndices}
\alias{PriceIndices}
\title{The list of package functions and their demonstration}
\description{
The \strong{PriceIndices} package is a tool for Bilateral and Multilateral Price Index Calculations. 
A demonstration of package functions is here: \href{ https://github.com/JacekBialek/important_documents/blob/main/README.pdf }{README}.
The package documentation can be found \href{https://github.com/JacekBialek/important_documents/blob/main/PriceIndices_manual.pdf}{HERE}.
The list of package functions is as follows:
}
\section{Data sets in the package and generating artificial scanner data sets}{

\code{\link{dataAGGR}}

\code{\link{dataMATCH}}

\code{\link{dataMARS}}

\code{\link{dataCOICOP}}

\code{\link{milk}} 

\code{\link{sugar}}

\code{\link{coffee}}

\code{\link{dataU}}

\code{\link{generate}}

\code{\link{generate_CES}}

\code{\link{tindex}}
}

\section{Functions for data processing}{

\code{\link{data_check}} 

\code{\link{data_preparing}} 

\code{\link{data_imputing}} 

\code{\link{data_aggregating}} 

\code{\link{data_unit}} 

\code{\link{data_norm}} 

\code{\link{data_selecting}}

\code{\link{data_classifying}}

\code{\link{model_classification}}

\code{\link{save_model}}

\code{\link{load_model}}

\code{\link{data_matching}} 

\code{\link{data_filtering}} 

\code{\link{data_reducing}} 

\code{\link{MARS}}
}

\section{Functions providing dataset characteristics}{

\code{\link{available}} 

\code{\link{matched}}

\code{\link{matched_index}}

\code{\link{matched_fig}}

\code{\link{products}}

\code{\link{products_fig}}

\code{\link{prices}} 

\code{\link{quantities}} 

\code{\link{sales}}

\code{\link{sales_groups}} 

\code{\link{sales_groups2}} 

\code{\link{expenditures}}

\code{\link{pqcor}}

\code{\link{pqcor_fig}}

\code{\link{dissimilarity_fig}}

\code{\link{elasticity}}

\code{\link{elasticity_fig}}
}

\section{Functions for bilateral unweighted price index calculation}{

\code{\link{bmw}} 

\code{\link{carli}}

\code{\link{cswd}}

\code{\link{dutot}}

\code{\link{jevons}}

\code{\link{harmonic}}
}

\section{Functions for bilateral weighted index calculation}{

\code{\link{agmean}} 

\code{\link{banajree}}

\code{\link{bialek}}

\code{\link{davies}}

\code{\link{drobisch}}

\code{\link{fisher}}

\code{\link{geary_khamis}}

\code{\link{geolaspeyres}}

\code{\link{geolowe}}

\code{\link{geopaasche}}

\code{\link{geoyoung}}

\code{\link{geohybrid}}

\code{\link{hybrid}}

\code{\link{laspeyres}}

\code{\link{lehr}}

\code{\link{lloyd_moulton}}

\code{\link{lowe}}

\code{\link{marshall_edgeworth}}

\code{\link{paasche}}

\code{\link{palgrave}}

\code{\link{sato_vartia}}

\code{\link{stuvel}}

\code{\link{tornqvist}}

\code{\link{vartia}}

\code{\link{walsh}}

\code{\link{young}}

\code{\link{QMp}}

\code{\link{IQMp}}

\code{\link{QMq}}

\code{\link{value_index}}

\code{\link{unit_value_index}}
}

\section{Functions for chain index calculation}{

\code{\link{chbmw}} 

\code{\link{chcarli}}

\code{\link{chcswd}}

\code{\link{chdutot}}

\code{\link{chjevons}}

\code{\link{chharmonic}}

\code{\link{chagmean}} 

\code{\link{chbanajree}}

\code{\link{chbialek}}

\code{\link{davies}}

\code{\link{chdrobisch}}

\code{\link{chfisher}}

\code{\link{chgeary_khamis}}

\code{\link{chgeolaspeyres}}

\code{\link{chgeolowe}}

\code{\link{chgeopaasche}}

\code{\link{chgeoyoung}}

\code{\link{chgeohybrid}}

\code{\link{chhybrid}}

\code{\link{chlaspeyres}}

\code{\link{chlehr}}

\code{\link{chlloyd_moulton}}

\code{\link{chlowe}}

\code{\link{chmarshall_edgeworth}}

\code{\link{chpaasche}}

\code{\link{chpalgrave}}

\code{\link{chsato_vartia}}

\code{\link{chstuvel}}

\code{\link{chtornqvist}}

\code{\link{chvartia}}

\code{\link{chwalsh}}

\code{\link{chyoung}}

\code{\link{chQMp}}

\code{\link{chIQMp}}

\code{\link{chQMq}}
}

\section{Functions for multilateral price index calculation}{

\code{\link{ccdi}} 

\code{\link{geks}}

\code{\link{wgeks}}

\code{\link{geksl}}

\code{\link{wgeksl}}

\code{\link{geksgl}}

\code{\link{wgeksgl}}

\code{\link{geksaqu}}

\code{\link{wgeksaqu}}

\code{\link{geksaqi}}

\code{\link{wgeksaqi}}

\code{\link{geksgaqi}}

\code{\link{wgeksgaqi}}

\code{\link{geksj}}

\code{\link{geksw}}

\code{\link{geksqm}}

\code{\link{geksiqm}}

\code{\link{gekslm}}

\code{\link{gk}}

\code{\link{QU}}

\code{\link{tpd}}

\code{\link{SPQ}}

\code{\link{m_decomposition}}
}

\section{Functions for extending multilateral price indices by using splicing methods}{

\code{\link{ccdi_splice}} 

\code{\link{geks_splice}}

\code{\link{wgeks_splice}}

\code{\link{geksj_splice}}

\code{\link{geksw_splice}}

\code{\link{geksl_splice}}

\code{\link{wgeksl_splice}}

\code{\link{geksgl_splice}}

\code{\link{wgeksgl_splice}}

\code{\link{geksaqu_splice}}

\code{\link{wgeksaqu_splice}}

\code{\link{geksaqi_splice}}

\code{\link{wgeksaqi_splice}}

\code{\link{geksgaqi_splice}}

\code{\link{wgeksgaqi_splice}}

\code{\link{geksqm_splice}}

\code{\link{geksiqm_splice}}

\code{\link{gekslm_splice}}

\code{\link{gk_splice}}

\code{\link{tpd_splice}}
}

\section{Functions for extending multilateral price indices by using the FBEW method}{

\code{\link{ccdi_fbew}} 

\code{\link{geks_fbew}}

\code{\link{wgeks_fbew}}

\code{\link{geksj_fbew}}

\code{\link{geksw_fbew}}

\code{\link{geksl_fbew}}

\code{\link{wgeksl_fbew}}

\code{\link{geksgl_fbew}}

\code{\link{wgeksgl_fbew}}

\code{\link{geksaqu_fbew}}

\code{\link{wgeksaqu_fbew}}

\code{\link{geksaqi_fbew}}

\code{\link{wgeksaqi_fbew}}

\code{\link{geksgaqi_fbew}}

\code{\link{wgeksgaqi_fbew}}

\code{\link{geksqm_fbew}}

\code{\link{geksiqm_fbew}}

\code{\link{gekslm_fbew}}

\code{\link{gk_fbew}}

\code{\link{tpd_fbew}}
}

\section{Functions for extending multilateral price indices by using the FBMW method}{

\code{\link{ccdi_fbmw}} 

\code{\link{geks_fbmw}}

\code{\link{wgeks_fbmw}}

\code{\link{geksj_fbmw}}

\code{\link{geksw_fbmw}}

\code{\link{geksl_fbmw}}

\code{\link{wgeksl_fbmw}}

\code{\link{geksgl_fbmw}}

\code{\link{wgeksgl_fbmw}}

\code{\link{geksaqu_fbmw}}

\code{\link{wgeksaqu_fbmw}}

\code{\link{geksaqi_fbmw}}

\code{\link{wgeksaqi_fbmw}}

\code{\link{geksgaqi_fbmw}}

\code{\link{wgeksgaqi_fbmw}}

\code{\link{geksqm_fbmw}}

\code{\link{geksiqm_fbmw}}

\code{\link{gekslm_fbmw}}

\code{\link{gk_fbmw}}

\code{\link{tpd_fbmw}}
}

\section{Functions for bilateral indicator calculations}{

\code{\link{bennet}} 

\code{\link{montgomery}}
}

\section{Functions for multilateral indicator calculations}{

\code{\link{mbennet}} 

\code{\link{mmontgomery}}
}

\section{General functions for price index calculations}{

\code{\link{price_indices}}

\code{\link{final_index}}
}

\section{Functions for comparisons of price indices}{

\code{\link{compare_indices_df}} 

\code{\link{compare_indices_list}}

\code{\link{compare_indices_jk}}

\code{\link{compare_distances}}

\code{\link{compare_to_target}}
}

