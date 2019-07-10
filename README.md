# Unsupervised Chinese terminology detection based on real-world statistics

we propose a new method which is based on Point Mutually Information, Branch Entropy and real-world data with over 16 billion Chinese characters. Our method has achieved better results in both precision and recall 

## Data

- **Real-World data**: 8 Gigabytes news data contains over 1.6 billion words which covers sports, entertainments, political, science, arts and culture from Sina, China news, Tencent, Baidu and People's Daily
- **Test Data**: Drug name data which is collected from 38 3A-grade hospitals.

## Method

```latex
PMI is
$$
\begin{align*}
  pmi(x,y) = \log_2{\frac{p(x,y)}{p((x)p(y)}}
\end{align*}
$$
Define $\theta$ as modification factor
$$
\begin{equation}
  \theta = \frac{p(x',y')}{p(x')p(y')}
\end{equation}
$$
PMI' can be define as
$$
pmi' = \log_2{\{\frac{p(x,y)}{p(x)p(y)}}\frac{1}{\theta}\}
$$
Normalized pmi'
$$
\mathcal{N}(pmi') = \frac{{pmi'}-\min{(pmi')}}{\max{(pmi')}-\min{(pmi')}}
$$
Branch Entropy
$$
\mbox{H}(x) = -\sum\limits_{i} p(x_i)\log_{2}{p(x_i)}
$$

$$
\mathcal{H}_{outer}(x) = \min \lbrace \mbox{H}_r(x),\mbox{H}_l(x) \rbrace \\
 \mathcal{H}_{outer}(x) = \min \lbrace \mbox{H}_r(x),\mbox{H}_l(x) \rbrace \\
 \mbox{BE}(x) = -\mathcal{H}_{outer}(x)+\mathcal{H}_{inner}(x) \\
$$

Normalization 
$$
\mathcal{N}(\mbox{BE}(x)) = \frac{\mbox{BE}(x)-\overline{\mbox{BE}(x)}}{\sigma\mbox{(BE(x)}}
$$
Final Score
$$
\mbox{Score}(x) = \lambda\mathcal{N}(\mbox{BE}(x)) + (1-\lambda)\mathcal{N}(pmi')
$$
```



## Result

|                                   | **Modifired PMI+BE** | **PMI**   |
| :-------------------------------- | -------------------- | --------- |
| Detected words                    | 1003                 | 271       |
| Correctly detected words          | 586                  | 105       |
| Words under criterion             | 390                  | 87        |
| Detected of Words under criterion | 322                  | 60        |
| **Precision**                     | **58.4%**            | **38.7%** |
| **Recall**                        | **82.6%**            | **70.0%** |

## Environment

- **Real world data**: ~8GB news data 
- **Test file**: ~700KB drug name data
-  **Experiment IDE**: RStudio Version 1.1.414
-  **OS**: ubuntu 14.04

