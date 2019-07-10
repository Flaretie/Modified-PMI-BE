# Unsupervised Chinese terminology detection based on real-world statistics

we propose a new method which is based on Point Mutually Information, Branch Entropy and real-world data with over 16 billion Chinese characters. Our method has achieved better results in both precision and recall 

## Data

- **Real-World data**: 8 Gigabytes news data contains over 1.6 billion words which covers sports, entertainments, political, science, arts and culture from Sina, China news, Tencent, Baidu and People's Daily
- **Test Data**: Drug name data which is collected from 38 3A-grade hospitals.

|                                   | **Modifired PMI+BE** | **PMI**   |
| :-------------------------------- | -------------------- | --------- |
| Detected words                    | 1003                 | 271       |
| Correctly detected words          | 586                  | 105       |
| Words under criterion             | 390                  | 87        |
| Detected of Words under criterion | 322                  | 60        |
| **Precision**                     | **58.4%**            | **38.7%** |
| **Recall**                        | **82.6%**            | **70.0%** |




