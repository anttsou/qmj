
---
title: "An Implementation of Quality Minus Junk with R"
tags:
- Quality Minus Junk
- Factor investing
- Systematic Investment
- R package
date: "20 January 2025"
output: pdf_document
authors:
- name: Anthoney Tsou
  email: anttsou@gmail.com
  affiliation: 3
- name: Eugene Choe
  email: ec7@williams.edu
  affiliation: 3
- name: David Kane
  email: david.kane@gmail.com
  affiliation: 2
- name: Ryan Kwon
  email: rynkwn@gmail.com
  affiliation: 3
- name: Yanrong Song
  orcid: "0009-0003-5738-8384"
  email: yrsong129@gmail.com
  affiliation: 1
- name: Zijie Zhu
  email: zijie.miller.zhu@gmail.com
  affiliation: 1



bibliography: paper.bib
affiliations:
- name: Columbia University, United States
  index: 1
- name: Harvard University, United States
  index: 2
- name: Williams College, United States
  index: 3
---



# Summary

The qmj \textbf{R} package implements the methodology and findings of the seminal paper Quality Minus Junk by Clifford Asness, Andrea Frazzini, and Lasse Pedersen. The paper introduces a framework for assessing company quality based on four key dimensions: profitability, growth, safety, and payouts. By combining these metrics, the methodology generates an overall quality score, which has been shown to predict market outperformance for high-quality companies and underperformance for low-quality ("junk") companies.

This package provides a practical implementation of the equations and techniques described in the paper, utilizing data scraped from publicly available online sources. It enables users to calculate quality scores for companies listed in the Russell 3000 Index, offering an accessible tool for researchers, investors, and portfolio managers to analyze and apply the Quality Minus Junk factor in their work. Users can also update these scores themselves with qmj functions that download necessary financial and stock data from Yahoo Finance.


# Statement of need

There is no single, widely accepted definition for the quality of a stock. MSCI's measure of quality combines the z-scores of three winsorized numbers: Return on Equity, Debt to Equity and Earnings Variability to calculate quality indices for their quality indexes. In ``Quality Minus Junk (QMJ)'', quality is defined as the scaled measure of four components: profitability, growth, safety, and payouts. These components respectively measure earnings relative to costs, change in profits over time, risk in future returns, and “shareholder friendliness”.The QMJ factor has gained significant traction among academics, practitioners, and portfolio managers as a reliable measure of a company's quality, which is shown to correlate with long-term outperformance in equity markets.
While the theoretical foundations and empirical results of QMJ are well-documented, there is a gap in tools that allow researchers and practitioners to seamlessly compute the QMJ factor for large-scale datasets.

The qmj \textbf{R} package addresses a critical need in the quantitative finance and investment research community by providing a reproducible and accessible implementation of the QMJ factor. 


\textbf{qmj} calculates the quality of publicly traded US stocks. The package uses the Russell 3000, an index composed of the 3000 largest US companies by market capitalization, as a sample universe. This universe was chosen for its relevance and reliable presence on Yahoo Finance, which \textbf{qmj} uses to automatically gather data. The size of companies in the Russell 3000 also reduces anomalous data points in our calculations.

# Citations

C. Asness, A. Frazzini, and L. Pedersen.  Quality minus junk.AQR Capital Management, 2014.

R.  Investments.   Russell  u.s  equity  indexes:  Construction  and  methodology,  2015.

MSCI. Msci quality indices methodology, May 2013.
