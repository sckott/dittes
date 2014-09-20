


## Summary of data

Prep data


```r
dat <- read.csv("dittes_data.csv")
dat$date <- as.Date(paste(as.character(dat$year), "-01-01", sep = ""), "%Y-%m-%d")
dat <- dat[ !dat$habitat == 2, ] # drop habitat=32
```

Make a `dplyr` `tbl_df` object


```r
library('dplyr')
```

```

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
```

```r
dat_df <- tbl_df(dat)
```

Define function to get quick summary by factor variable


```r
avg_by <- function(...){
  dat_df %>% 
    group_by(...) %>%
    summarise(rdm_mean = mean(rdm, na.rm = TRUE),
              rdm_sd = sd(rdm, na.rm = TRUE),
              rdm_n = length(na.omit(rdm)))
}
```

by transect


```r
avg_by(transect)
```

```
Source: local data frame [12 x 4]

   transect rdm_mean rdm_sd rdm_n
1      AD10   13.464 11.425    84
2      AD11   18.476 18.032    85
3      AD12   24.394 62.317    85
4       AL1    9.419  7.072    80
5       BK5    8.400  6.887    70
6       BL7   16.776 16.636    85
7       ML9   15.676 19.136    85
8       NF6   11.467  8.023    75
9       NV8   12.461 11.599    85
10      PK2   10.693  6.775    75
11      PL3    7.255  6.864    85
12      PL4    7.518  5.031    85
```

by year


```r
avg_by(year)
```

```
Source: local data frame [10 x 4]

   year rdm_mean rdm_sd rdm_n
1  1997   26.425 77.092    60
2  1998   18.875 18.395    60
3  1999   20.240 19.207    50
4  2000   12.676 10.337   110
5  2001   15.217  9.473   120
6  2002   10.558  7.694   120
7  2003    7.277  4.077   110
8  2004    8.871  5.321   120
9  2005   17.118 13.261   110
10 2014    7.450  6.253   119
```

by soil type


```r
avg_by(soil)
```

```
Source: local data frame [9 x 4]

  soil rdm_mean  rdm_sd rdm_n
1   Kc   10.693   6.775    75
2  SuD   16.837  13.170    46
3  TfD   13.256   9.678    41
4  TgD   14.725  16.469   257
5  TgE   40.017 103.077    30
6  ThE   15.268  13.511   200
7  ToE    9.650   3.866    10
8  TuB    7.682   6.275   240
9  Txc    9.419   7.072    80
```

by slope_class


```r
avg_by(slope_class)
```

```
Source: local data frame [4 x 4]

  slope_class rdm_mean rdm_sd rdm_n
1           1   11.341 12.819   523
2           2   12.752 10.555   204
3           3   17.598 38.776   240
4           4    7.958  6.283    12
```

by landform


```r
avg_by(landform)
```

```
Source: local data frame [3 x 4]

    landform rdm_mean rdm_sd rdm_n
1   Foothill   16.903 31.292   424
2 Frontslope   14.287 13.537   160
3     Plains    8.606  6.634   395
```

by landform


```r
avg_by(landform)
```

```
Source: local data frame [3 x 4]

    landform rdm_mean rdm_sd rdm_n
1   Foothill   16.903 31.292   424
2 Frontslope   14.287 13.537   160
3     Plains    8.606  6.634   395
```

by habitat


```r
avg_by(habitat)
```

```
Source: local data frame [2 x 4]

  habitat rdm_mean rdm_sd rdm_n
1       1    12.35  24.24   700
2       3    15.08  15.05   279
```

All combinators of factors


```r
avg_by(transect, year, soil, slope_class, landform, habitat) %>% data.frame
```

```
    transect year soil slope_class   landform habitat rdm_mean   rdm_sd
1       AD10 1997  SuD           2   Foothill       1    8.000       NA
2       AD10 1997  SuD           3   Foothill       3    5.250   5.3033
3       AD10 1997  ThE           1   Foothill       3   35.000  11.3137
4       AD10 1997  ThE           3   Foothill       1      NaN      NaN
5       AD10 1997  ThE           3   Foothill       3      NaN      NaN
6       AD10 1998  SuD           2   Foothill       1    8.000       NA
7       AD10 1998  SuD           3   Foothill       3   14.250   3.1820
8       AD10 1998  ThE           1   Foothill       3   52.250  25.8094
9       AD10 1998  ThE           3   Foothill       1      NaN      NaN
10      AD10 1998  ThE           3   Foothill       3      NaN      NaN
11      AD10 1999  SuD           2   Foothill       1    5.500       NA
12      AD10 1999  SuD           3   Foothill       3   10.000   2.1213
13      AD10 1999  ThE           1   Foothill       3   17.250   9.5459
14      AD10 1999  ThE           3   Foothill       1      NaN      NaN
15      AD10 1999  ThE           3   Foothill       3      NaN      NaN
16      AD10 2000  SuD           2   Foothill       1   13.750  15.2028
17      AD10 2000  SuD           3   Foothill       3   22.500  21.9203
18      AD10 2000  ThE           1   Foothill       3    9.750  11.6673
19      AD10 2000  ThE           3   Foothill       1   14.500   7.7782
20      AD10 2000  ThE           3   Foothill       3   14.000   5.6569
21      AD10 2001  SuD           2   Foothill       1   20.500  12.0208
22      AD10 2001  SuD           3   Foothill       3   11.500   7.7782
23      AD10 2001  ThE           1   Foothill       3    4.500   3.5355
24      AD10 2001  ThE           3   Foothill       1   13.500   9.1924
25      AD10 2001  ThE           3   Foothill       3    4.500   2.1213
26      AD10 2002  SuD           2   Foothill       1   10.750   7.4246
27      AD10 2002  SuD           3   Foothill       3   10.000   9.8995
28      AD10 2002  ThE           1   Foothill       3   11.500  12.0208
29      AD10 2002  ThE           3   Foothill       1   17.000  11.3137
30      AD10 2002  ThE           3   Foothill       3   16.000   4.2426
31      AD10 2003  SuD           2   Foothill       1    6.250   8.1317
32      AD10 2003  SuD           3   Foothill       3    6.000   2.8284
33      AD10 2003  ThE           1   Foothill       3    6.500   2.1213
34      AD10 2003  ThE           3   Foothill       1    3.250   1.0607
35      AD10 2003  ThE           3   Foothill       3    4.500   4.9497
36      AD10 2004  SuD           2   Foothill       1   10.750   3.8891
37      AD10 2004  SuD           3   Foothill       3    6.250   2.4749
38      AD10 2004  ThE           1   Foothill       3    4.000   2.8284
39      AD10 2004  ThE           3   Foothill       1    9.000   4.2426
40      AD10 2004  ThE           3   Foothill       3    7.500   3.5355
41      AD10 2005  SuD           2   Foothill       1   20.000   7.0711
42      AD10 2005  SuD           3   Foothill       3   33.500  20.5061
43      AD10 2005  ThE           1   Foothill       3    4.000   4.2426
44      AD10 2005  ThE           3   Foothill       1   20.000   7.0711
45      AD10 2005  ThE           3   Foothill       3   17.500  10.6066
46      AD10 2014  SuD           2   Foothill       1   15.000       NA
47      AD10 2014  SuD           3   Foothill       3    9.750   2.4749
48      AD10 2014  ThE           1   Foothill       3   21.250   0.3536
49      AD10 2014  ThE           3   Foothill       1   13.750   2.4749
50      AD10 2014  ThE           3   Foothill       3   15.250   0.3536
51      AD11 1997  TgD           1   Foothill       1   26.000       NA
52      AD11 1997  TgD           1   Foothill       3    7.500       NA
53      AD11 1997  TgD           2   Foothill       3      NaN      NaN
54      AD11 1997  TgD           3   Foothill       3      NaN      NaN
55      AD11 1997  ThE           1   Foothill       3   50.250  44.1942
56      AD11 1997  ThE           2   Foothill       3   89.000       NA
57      AD11 1998  TgD           1   Foothill       1   20.500       NA
58      AD11 1998  TgD           1   Foothill       3   46.500       NA
59      AD11 1998  TgD           2   Foothill       3      NaN      NaN
60      AD11 1998  TgD           3   Foothill       3      NaN      NaN
61      AD11 1998  ThE           1   Foothill       3   21.750   1.7678
62      AD11 1998  ThE           2   Foothill       3   20.000       NA
63      AD11 1999  TgD           1   Foothill       1   97.500       NA
64      AD11 1999  TgD           1   Foothill       3   25.000       NA
65      AD11 1999  TgD           2   Foothill       3      NaN      NaN
66      AD11 1999  TgD           3   Foothill       3      NaN      NaN
67      AD11 1999  ThE           1   Foothill       3   41.500  14.8492
68      AD11 1999  ThE           2   Foothill       3   18.000       NA
69      AD11 2000  TgD           1   Foothill       1   14.000       NA
70      AD11 2000  TgD           1   Foothill       3   24.500   7.7782
71      AD11 2000  TgD           2   Foothill       3   30.000   1.4142
72      AD11 2000  TgD           3   Foothill       3   18.000  16.9706
73      AD11 2000  ThE           1   Foothill       3   30.000  33.9411
74      AD11 2000  ThE           2   Foothill       3   37.500       NA
75      AD11 2001  TgD           1   Foothill       1   43.000       NA
76      AD11 2001  TgD           1   Foothill       3   22.500   0.7071
77      AD11 2001  TgD           2   Foothill       3   21.000   4.2426
78      AD11 2001  TgD           3   Foothill       3   31.000   1.4142
79      AD11 2001  ThE           1   Foothill       3   20.000  16.9706
80      AD11 2001  ThE           2   Foothill       3   24.000       NA
81      AD11 2002  TgD           1   Foothill       1   16.000       NA
82      AD11 2002  TgD           1   Foothill       3   16.000   7.0711
83      AD11 2002  TgD           2   Foothill       3   18.500   9.1924
84      AD11 2002  TgD           3   Foothill       3   12.500   3.5355
85      AD11 2002  ThE           1   Foothill       3   12.500   7.7782
86      AD11 2002  ThE           2   Foothill       3   18.000       NA
87      AD11 2003  TgD           1   Foothill       1    6.000       NA
88      AD11 2003  TgD           1   Foothill       3    3.500   4.9497
89      AD11 2003  TgD           2   Foothill       3    3.000   0.0000
90      AD11 2003  TgD           3   Foothill       3    3.250   1.0607
91      AD11 2003  ThE           1   Foothill       3    8.500   4.9497
92      AD11 2003  ThE           2   Foothill       3    5.000       NA
93      AD11 2004  TgD           1   Foothill       1    8.000       NA
94      AD11 2004  TgD           1   Foothill       3   12.500  10.6066
95      AD11 2004  TgD           2   Foothill       3    7.500   3.5355
96      AD11 2004  TgD           3   Foothill       3   11.500   9.1924
97      AD11 2004  ThE           1   Foothill       3   18.000   2.8284
98      AD11 2004  ThE           2   Foothill       3    9.000       NA
99      AD11 2005  TgD           1   Foothill       1   12.000       NA
100     AD11 2005  TgD           1   Foothill       3   16.000   5.6569
101     AD11 2005  TgD           2   Foothill       3    9.000   1.4142
102     AD11 2005  TgD           3   Foothill       3    8.500   0.7071
103     AD11 2005  ThE           1   Foothill       3    6.000   0.0000
104     AD11 2005  ThE           2   Foothill       3   28.000       NA
105     AD11 2014  TgD           1   Foothill       1    2.000       NA
106     AD11 2014  TgD           1   Foothill       3    4.000   2.1213
107     AD11 2014  TgD           2   Foothill       3    6.250   1.0607
108     AD11 2014  TgD           3   Foothill       3    3.250   1.7678
109     AD11 2014  ThE           1   Foothill       3    5.000   0.7071
110     AD11 2014  ThE           2   Foothill       3    6.500       NA
111     AD12 1997  SuD           3   Foothill       1   44.000       NA
112     AD12 1997  TgE           2   Foothill       1    8.000       NA
113     AD12 1997  TgE           3   Foothill       1  316.000 373.3524
114     AD12 1997  ThE           3   Foothill       1      NaN      NaN
115     AD12 1997  ThE           3   Foothill       3      NaN      NaN
116     AD12 1997  ToE           3   Foothill       1    6.000       NA
117     AD12 1998  SuD           3   Foothill       1   28.000       NA
118     AD12 1998  TgE           2   Foothill       1   33.500       NA
119     AD12 1998  TgE           3   Foothill       1   15.250   9.5459
120     AD12 1998  ThE           3   Foothill       1      NaN      NaN
121     AD12 1998  ThE           3   Foothill       3      NaN      NaN
122     AD12 1998  ToE           3   Foothill       1   16.500       NA
123     AD12 1999  SuD           3   Foothill       1   26.000       NA
124     AD12 1999  TgE           2   Foothill       1   15.000       NA
125     AD12 1999  TgE           3   Foothill       1   40.750  30.0520
126     AD12 1999  ThE           3   Foothill       1      NaN      NaN
127     AD12 1999  ThE           3   Foothill       3      NaN      NaN
128     AD12 1999  ToE           3   Foothill       1    9.000       NA
129     AD12 2000  SuD           3   Foothill       1   38.000       NA
130     AD12 2000  TgE           2   Foothill       1   16.000       NA
131     AD12 2000  TgE           3   Foothill       1   23.000   4.2426
132     AD12 2000  ThE           3   Foothill       1   13.667   5.5076
133     AD12 2000  ThE           3   Foothill       3   26.000   2.8284
134     AD12 2000  ToE           3   Foothill       1    5.000       NA
135     AD12 2001  SuD           3   Foothill       1   40.000       NA
136     AD12 2001  TgE           2   Foothill       1   20.000       NA
137     AD12 2001  TgE           3   Foothill       1   23.000   4.2426
138     AD12 2001  ThE           3   Foothill       1   23.000   3.0000
139     AD12 2001  ThE           3   Foothill       3    4.500   0.7071
140     AD12 2001  ToE           3   Foothill       1    5.000       NA
141     AD12 2002  SuD           3   Foothill       1   44.000       NA
142     AD12 2002  TgE           2   Foothill       1   18.000       NA
143     AD12 2002  TgE           3   Foothill       1   17.000   1.4142
144     AD12 2002  ThE           3   Foothill       1   11.333   2.5166
145     AD12 2002  ThE           3   Foothill       3    7.500   3.5355
146     AD12 2002  ToE           3   Foothill       1   11.000       NA
147     AD12 2003  SuD           3   Foothill       1   24.500       NA
148     AD12 2003  TgE           2   Foothill       1   11.000       NA
149     AD12 2003  TgE           3   Foothill       1   10.000   5.6569
150     AD12 2003  ThE           3   Foothill       1    8.000   1.0000
151     AD12 2003  ThE           3   Foothill       3    5.000   2.8284
152     AD12 2003  ToE           3   Foothill       1    7.000       NA
153     AD12 2004  SuD           3   Foothill       1   10.000       NA
154     AD12 2004  TgE           2   Foothill       1    1.000       NA
155     AD12 2004  TgE           3   Foothill       1   13.000   2.8284
156     AD12 2004  ThE           3   Foothill       1    7.333   3.0551
157     AD12 2004  ThE           3   Foothill       3   13.500   2.1213
158     AD12 2004  ToE           3   Foothill       1   12.000       NA
159     AD12 2005  SuD           3   Foothill       1   47.000       NA
160     AD12 2005  TgE           2   Foothill       1   33.500       NA
161     AD12 2005  TgE           3   Foothill       1   49.000  11.3137
162     AD12 2005  ThE           3   Foothill       1   15.000   5.0000
163     AD12 2005  ThE           3   Foothill       3   27.500   3.5355
164     AD12 2005  ToE           3   Foothill       1   12.500       NA
165     AD12 2014  SuD           3   Foothill       1   14.500       NA
166     AD12 2014  TgE           2   Foothill       1   15.000       NA
167     AD12 2014  TgE           3   Foothill       1    7.750   7.4246
168     AD12 2014  ThE           3   Foothill       1   10.500   8.8882
169     AD12 2014  ThE           3   Foothill       3   13.000   6.3640
170     AD12 2014  ToE           3   Foothill       1   12.500       NA
171      AL1 1997  Txc           1     Plains       1    2.750   1.7678
172      AL1 1997  Txc           2     Plains       1   13.000  12.9711
173      AL1 1998  Txc           1     Plains       1    4.500   0.0000
174      AL1 1998  Txc           2     Plains       1   14.167   5.7951
175      AL1 1999  Txc           1     Plains       1      NaN      NaN
176      AL1 1999  Txc           2     Plains       1      NaN      NaN
177      AL1 2000  Txc           1     Plains       1    6.500   3.5355
178      AL1 2000  Txc           2     Plains       1    6.438   2.3970
179      AL1 2001  Txc           1     Plains       1    4.500   2.1213
180      AL1 2001  Txc           2     Plains       1    9.000   2.6186
181      AL1 2002  Txc           1     Plains       1    5.500   2.1213
182      AL1 2002  Txc           2     Plains       1   14.000   6.0945
183      AL1 2003  Txc           1     Plains       1    4.500   0.7071
184      AL1 2003  Txc           2     Plains       1    8.500   1.3093
185      AL1 2004  Txc           1     Plains       1    2.500   2.1213
186      AL1 2004  Txc           2     Plains       1   10.750   7.3824
187      AL1 2005  Txc           1     Plains       1   17.750   7.4246
188      AL1 2005  Txc           2     Plains       1   18.250  10.9642
189      AL1 2014  Txc           1     Plains       1    8.000   2.8284
190      AL1 2014  Txc           2     Plains       1    2.938   1.8213
191      BK5 1997  TuB           1     Plains       1    6.500   6.4711
192      BK5 1998  TuB           1     Plains       1   11.500   3.1024
193      BK5 1999  TuB           1     Plains       1      NaN      NaN
194      BK5 2000  TuB           1     Plains       1      NaN      NaN
195      BK5 2001  TuB           1     Plains       1   11.050   3.3784
196      BK5 2002  TuB           1     Plains       1    6.150   7.7998
197      BK5 2003  TuB           1     Plains       1    7.950   8.5325
198      BK5 2004  TuB           1     Plains       1    8.300   7.8181
199      BK5 2005  TuB           1     Plains       1   13.800   6.2503
200      BK5 2014  TuB           1     Plains       1    2.550   2.0062
201      BL7 1997  TgD           2 Frontslope       1    7.000       NA
202      BL7 1997  TgD           2 Frontslope       3      NaN      NaN
203      BL7 1997  TgD           3 Frontslope       1   18.000  14.6913
204      BL7 1998  TgD           2 Frontslope       1   33.500       NA
205      BL7 1998  TgD           2 Frontslope       3      NaN      NaN
206      BL7 1998  TgD           3 Frontslope       1   36.500  49.6404
207      BL7 1999  TgD           2 Frontslope       1   19.000       NA
208      BL7 1999  TgD           2 Frontslope       3      NaN      NaN
209      BL7 1999  TgD           3 Frontslope       1   15.125   9.3930
210      BL7 2000  TgD           2 Frontslope       1   18.250   2.3629
211      BL7 2000  TgD           2 Frontslope       3   26.500   4.9497
212      BL7 2000  TgD           3 Frontslope       1   17.375  13.8827
213      BL7 2001  TgD           2 Frontslope       1   20.500   9.0370
214      BL7 2001  TgD           2 Frontslope       3   12.500  10.6066
215      BL7 2001  TgD           3 Frontslope       1   14.250   9.2871
216      BL7 2002  TgD           2 Frontslope       1   15.875   3.0104
217      BL7 2002  TgD           2 Frontslope       3   11.000   6.3640
218      BL7 2002  TgD           3 Frontslope       1   15.750   6.4485
219      BL7 2003  TgD           2 Frontslope       1    8.750   1.5000
220      BL7 2003  TgD           2 Frontslope       3    7.500   0.7071
221      BL7 2003  TgD           3 Frontslope       1    8.250   5.1881
222      BL7 2004  TgD           2 Frontslope       1    7.750   3.0957
223      BL7 2004  TgD           2 Frontslope       3    8.000   2.8284
224      BL7 2004  TgD           3 Frontslope       1    8.875   4.9728
225      BL7 2005  TgD           2 Frontslope       1   28.000  28.1425
226      BL7 2005  TgD           2 Frontslope       3    9.000   1.4142
227      BL7 2005  TgD           3 Frontslope       1   46.375  19.8888
228      BL7 2014  TgD           2 Frontslope       1    3.375   0.9465
229      BL7 2014  TgD           2 Frontslope       3   12.750   2.4749
230      BL7 2014  TgD           3 Frontslope       1   15.000  18.7617
231      ML9 1997  TfD           1   Foothill       1      NaN       NA
232      ML9 1997  TfD           1   Foothill       3   21.000   4.2426
233      ML9 1997  TgD           1   Foothill       1   14.000       NA
234      ML9 1997  TgD           1   Foothill       3   85.750 101.4698
235      ML9 1998  TfD           1   Foothill       1      NaN       NA
236      ML9 1998  TfD           1   Foothill       3   15.250   6.0104
237      ML9 1998  TgD           1   Foothill       1   49.000       NA
238      ML9 1998  TgD           1   Foothill       3   12.750   8.8388
239      ML9 1999  TfD           1   Foothill       1      NaN       NA
240      ML9 1999  TfD           1   Foothill       3   18.250   7.4246
241      ML9 1999  TgD           1   Foothill       1   16.500       NA
242      ML9 1999  TgD           1   Foothill       3   50.750   7.4246
243      ML9 2000  TfD           1   Foothill       1   14.000       NA
244      ML9 2000  TfD           1   Foothill       3   10.000   3.7417
245      ML9 2000  TgD           1   Foothill       1    2.500       NA
246      ML9 2000  TgD           1   Foothill       3   15.750   5.3619
247      ML9 2001  TfD           1   Foothill       1   11.000       NA
248      ML9 2001  TfD           1   Foothill       3   26.750  22.3215
249      ML9 2001  TgD           1   Foothill       1   47.000       NA
250      ML9 2001  TgD           1   Foothill       3   19.375   3.3510
251      ML9 2002  TfD           1   Foothill       1    8.000       NA
252      ML9 2002  TfD           1   Foothill       3   16.000   8.9815
253      ML9 2002  TgD           1   Foothill       1    7.000       NA
254      ML9 2002  TgD           1   Foothill       3    7.125   2.2500
255      ML9 2003  TfD           1   Foothill       1   12.000       NA
256      ML9 2003  TfD           1   Foothill       3    7.750   1.7078
257      ML9 2003  TgD           1   Foothill       1    3.000       NA
258      ML9 2003  TgD           1   Foothill       3    8.500   1.2910
259      ML9 2004  TfD           1   Foothill       1   10.000       NA
260      ML9 2004  TfD           1   Foothill       3   13.750   9.6393
261      ML9 2004  TgD           1   Foothill       1    4.000       NA
262      ML9 2004  TgD           1   Foothill       3   14.500   4.7958
263      ML9 2005  TfD           1   Foothill       1    7.000       NA
264      ML9 2005  TfD           1   Foothill       3   11.250   5.1235
265      ML9 2005  TgD           1   Foothill       1    9.000       NA
266      ML9 2005  TgD           1   Foothill       3    7.000   0.8165
267      ML9 2014  TfD           1   Foothill       1    5.000       NA
268      ML9 2014  TfD           1   Foothill       3    6.375   3.3260
269      ML9 2014  TgD           1   Foothill       1    6.500       NA
270      ML9 2014  TgD           1   Foothill       3   10.750   5.9652
271      NF6 1997  TgD           3 Frontslope       1      NaN       NA
272      NF6 1997  TgD           4 Frontslope       1      NaN      NaN
273      NF6 1997  ThE           3 Frontslope       1   13.333   9.8658
274      NF6 1997  ThE           3 Frontslope       3   13.750  15.2028
275      NF6 1998  TgD           3 Frontslope       1      NaN       NA
276      NF6 1998  TgD           4 Frontslope       1      NaN      NaN
277      NF6 1998  ThE           3 Frontslope       1   17.000  10.9659
278      NF6 1998  ThE           3 Frontslope       3   17.500  10.6066
279      NF6 1999  TgD           3 Frontslope       1      NaN       NA
280      NF6 1999  TgD           4 Frontslope       1      NaN      NaN
281      NF6 1999  ThE           3 Frontslope       1   29.500  10.0374
282      NF6 1999  ThE           3 Frontslope       3   12.000   1.4142
283      NF6 2000  TgD           3 Frontslope       1   11.000       NA
284      NF6 2000  TgD           4 Frontslope       1    4.500   0.7071
285      NF6 2000  ThE           3 Frontslope       1   16.833  15.7348
286      NF6 2000  ThE           3 Frontslope       3    9.375   9.0127
287      NF6 2001  TgD           3 Frontslope       1   11.000       NA
288      NF6 2001  TgD           4 Frontslope       1   19.500   4.9497
289      NF6 2001  ThE           3 Frontslope       1   14.333   3.2146
290      NF6 2001  ThE           3 Frontslope       3   16.750   3.4034
291      NF6 2002  TgD           3 Frontslope       1    2.500       NA
292      NF6 2002  TgD           4 Frontslope       1    1.500   0.7071
293      NF6 2002  ThE           3 Frontslope       1    4.167   1.0408
294      NF6 2002  ThE           3 Frontslope       3   10.750  10.9430
295      NF6 2003  TgD           3 Frontslope       1   10.000       NA
296      NF6 2003  TgD           4 Frontslope       1    8.000   1.4142
297      NF6 2003  ThE           3 Frontslope       1    8.333   2.0817
298      NF6 2003  ThE           3 Frontslope       3    8.000   2.4495
299      NF6 2004  TgD           3 Frontslope       1    7.000       NA
300      NF6 2004  TgD           4 Frontslope       1    7.000   1.4142
301      NF6 2004  ThE           3 Frontslope       1    8.833   4.0104
302      NF6 2004  ThE           3 Frontslope       3   11.500   3.7859
303      NF6 2005  TgD           3 Frontslope       1      NaN       NA
304      NF6 2005  TgD           4 Frontslope       1      NaN      NaN
305      NF6 2005  ThE           3 Frontslope       1      NaN      NaN
306      NF6 2005  ThE           3 Frontslope       3      NaN      NaN
307      NF6 2014  TgD           3 Frontslope       1    1.500       NA
308      NF6 2014  TgD           4 Frontslope       1    7.250   5.3033
309      NF6 2014  ThE           3 Frontslope       1   11.167   3.0139
310      NF6 2014  ThE           3 Frontslope       3    9.750   3.9686
311      NV8 1997  TgD           1   Foothill       1      NaN      NaN
312      NV8 1997  TgD           2   Foothill       1    4.000       NA
313      NV8 1997  TgD           2   Foothill       3    2.000       NA
314      NV8 1997  ThE           1   Foothill       1    5.167   3.7528
315      NV8 1998  TgD           1   Foothill       1      NaN      NaN
316      NV8 1998  TgD           2   Foothill       1    7.500       NA
317      NV8 1998  TgD           2   Foothill       3    8.000       NA
318      NV8 1998  ThE           1   Foothill       1   36.167  25.9631
319      NV8 1999  TgD           1   Foothill       1      NaN      NaN
320      NV8 1999  TgD           2   Foothill       1   10.000       NA
321      NV8 1999  TgD           2   Foothill       3    1.500       NA
322      NV8 1999  ThE           1   Foothill       1   28.000  22.6053
323      NV8 2000  TgD           1   Foothill       1    0.350   0.2121
324      NV8 2000  TgD           2   Foothill       1    8.500   6.3640
325      NV8 2000  TgD           2   Foothill       3   17.333  10.0167
326      NV8 2000  ThE           1   Foothill       1   18.000   5.2915
327      NV8 2001  TgD           1   Foothill       1    6.000   1.4142
328      NV8 2001  TgD           2   Foothill       1    5.500   3.5355
329      NV8 2001  TgD           2   Foothill       3    8.000   2.6458
330      NV8 2001  ThE           1   Foothill       1   16.333  15.2753
331      NV8 2002  TgD           1   Foothill       1    8.000   0.0000
332      NV8 2002  TgD           2   Foothill       1    5.750   1.0607
333      NV8 2002  TgD           2   Foothill       3   10.000   1.7321
334      NV8 2002  ThE           1   Foothill       1   24.000  12.1655
335      NV8 2003  TgD           1   Foothill       1    4.500   0.7071
336      NV8 2003  TgD           2   Foothill       1    7.500   0.7071
337      NV8 2003  TgD           2   Foothill       3    9.667   2.5166
338      NV8 2003  ThE           1   Foothill       1    6.000   3.4641
339      NV8 2004  TgD           1   Foothill       1    6.000   2.8284
340      NV8 2004  TgD           2   Foothill       1   14.500   3.5355
341      NV8 2004  TgD           2   Foothill       3   10.333   8.3865
342      NV8 2004  ThE           1   Foothill       1    3.667   2.5166
343      NV8 2005  TgD           1   Foothill       1   14.000   8.4853
344      NV8 2005  TgD           2   Foothill       1    9.500   0.7071
345      NV8 2005  TgD           2   Foothill       3   24.667  12.6623
346      NV8 2005  ThE           1   Foothill       1   28.000  15.3948
347      NV8 2014  TgD           1   Foothill       1    8.750   0.3536
348      NV8 2014  TgD           2   Foothill       1    8.500   8.4853
349      NV8 2014  TgD           2   Foothill       3   18.167  11.6440
350      NV8 2014  ThE           1   Foothill       1    7.000   4.5000
351      PK2 1997   Kc           1     Plains       1    7.300   4.5083
352      PK2 1997   Kc           2     Plains       1      NaN       NA
353      PK2 1998   Kc           1     Plains       1   15.700  10.3296
354      PK2 1998   Kc           2     Plains       1      NaN       NA
355      PK2 1999   Kc           1     Plains       1    9.600   3.3053
356      PK2 1999   Kc           2     Plains       1      NaN       NA
357      PK2 2000   Kc           1     Plains       1    8.889   4.3788
358      PK2 2000   Kc           2     Plains       1    3.000       NA
359      PK2 2001   Kc           1     Plains       1   16.556   7.6503
360      PK2 2001   Kc           2     Plains       1   18.000       NA
361      PK2 2002   Kc           1     Plains       1   11.222   5.1908
362      PK2 2002   Kc           2     Plains       1    6.000       NA
363      PK2 2003   Kc           1     Plains       1      NaN      NaN
364      PK2 2003   Kc           2     Plains       1      NaN       NA
365      PK2 2004   Kc           1     Plains       1    7.722   2.3333
366      PK2 2004   Kc           2     Plains       1   12.000       NA
367      PK2 2005   Kc           1     Plains       1   16.278   8.0472
368      PK2 2005   Kc           2     Plains       1   10.000       NA
369      PK2 2014   Kc           1     Plains       1    4.444   1.7038
370      PK2 2014   Kc           2     Plains       1    4.000       NA
371      PL3 1997  TuB           1     Plains       1    4.800   4.3960
372      PL3 1998  TuB           1     Plains       1    5.000   3.2787
373      PL3 1999  TuB           1     Plains       1   14.500  22.9728
374      PL3 2000  TuB           1     Plains       1    3.670   1.0111
375      PL3 2001  TuB           1     Plains       1   13.250   1.7834
376      PL3 2002  TuB           1     Plains       1    4.250   1.8597
377      PL3 2003  TuB           1     Plains       1    5.600   2.6331
378      PL3 2004  TuB           1     Plains       1    7.000   3.6286
379      PL3 2005  TuB           1     Plains       1   11.650   5.1857
380      PL3 2014  TuB           1     Plains       1    4.100   1.3904
381      PL4 1997  TuB           1     Plains       1   10.200  17.0426
382      PL4 1998  TuB           1     Plains       1    9.100   3.2094
383      PL4 1999  TuB           1     Plains       1    6.900   4.7090
384      PL4 2000  TuB           1     Plains       1    6.000   3.3993
385      PL4 2001  TuB           1     Plains       1   11.850   1.9444
386      PL4 2002  TuB           1     Plains       1    4.700   1.4757
387      PL4 2003  TuB           1     Plains       1    7.400   2.2706
388      PL4 2004  TuB           1     Plains       1    6.400   3.6878
389      PL4 2005  TuB           1     Plains       1    8.100   2.1318
390      PL4 2014  TuB           1     Plains       1    6.350   3.2834
    rdm_n
1       1
2       2
3       2
4       0
5       0
6       1
7       2
8       2
9       0
10      0
11      1
12      2
13      2
14      0
15      0
16      2
17      2
18      2
19      2
20      2
21      2
22      2
23      2
24      2
25      2
26      2
27      2
28      2
29      2
30      2
31      2
32      2
33      2
34      2
35      2
36      2
37      2
38      2
39      2
40      2
41      2
42      2
43      2
44      2
45      2
46      1
47      2
48      2
49      2
50      2
51      1
52      1
53      0
54      0
55      2
56      1
57      1
58      1
59      0
60      0
61      2
62      1
63      1
64      1
65      0
66      0
67      2
68      1
69      1
70      2
71      2
72      2
73      2
74      1
75      1
76      2
77      2
78      2
79      2
80      1
81      1
82      2
83      2
84      2
85      2
86      1
87      1
88      2
89      2
90      2
91      2
92      1
93      1
94      2
95      2
96      2
97      2
98      1
99      1
100     2
101     2
102     2
103     2
104     1
105     1
106     2
107     2
108     2
109     2
110     1
111     1
112     1
113     2
114     0
115     0
116     1
117     1
118     1
119     2
120     0
121     0
122     1
123     1
124     1
125     2
126     0
127     0
128     1
129     1
130     1
131     2
132     3
133     2
134     1
135     1
136     1
137     2
138     3
139     2
140     1
141     1
142     1
143     2
144     3
145     2
146     1
147     1
148     1
149     2
150     3
151     2
152     1
153     1
154     1
155     2
156     3
157     2
158     1
159     1
160     1
161     2
162     3
163     2
164     1
165     1
166     1
167     2
168     3
169     2
170     1
171     2
172     3
173     2
174     3
175     0
176     0
177     2
178     8
179     2
180     8
181     2
182     8
183     2
184     8
185     2
186     8
187     2
188     8
189     2
190     8
191     5
192     5
193     0
194     0
195    10
196    10
197    10
198    10
199    10
200    10
201     1
202     0
203     4
204     1
205     0
206     4
207     1
208     0
209     4
210     4
211     2
212     4
213     4
214     2
215     4
216     4
217     2
218     4
219     4
220     2
221     4
222     4
223     2
224     4
225     4
226     2
227     4
228     4
229     2
230     4
231     0
232     2
233     1
234     2
235     0
236     2
237     1
238     2
239     0
240     2
241     1
242     2
243     1
244     4
245     1
246     4
247     1
248     4
249     1
250     4
251     1
252     4
253     1
254     4
255     1
256     4
257     1
258     4
259     1
260     4
261     1
262     4
263     1
264     4
265     1
266     4
267     1
268     4
269     1
270     4
271     0
272     0
273     3
274     2
275     0
276     0
277     3
278     2
279     0
280     0
281     3
282     2
283     1
284     2
285     3
286     4
287     1
288     2
289     3
290     4
291     1
292     2
293     3
294     4
295     1
296     2
297     3
298     4
299     1
300     2
301     3
302     4
303     0
304     0
305     0
306     0
307     1
308     2
309     3
310     4
311     0
312     1
313     1
314     3
315     0
316     1
317     1
318     3
319     0
320     1
321     1
322     3
323     2
324     2
325     3
326     3
327     2
328     2
329     3
330     3
331     2
332     2
333     3
334     3
335     2
336     2
337     3
338     3
339     2
340     2
341     3
342     3
343     2
344     2
345     3
346     3
347     2
348     2
349     3
350     3
351     5
352     0
353     5
354     0
355     5
356     0
357     9
358     1
359     9
360     1
361     9
362     1
363     0
364     0
365     9
366     1
367     9
368     1
369     9
370     1
371     5
372     5
373     5
374    10
375    10
376    10
377    10
378    10
379    10
380    10
381     5
382     5
383     5
384    10
385    10
386    10
387    10
388    10
389    10
390    10
```
