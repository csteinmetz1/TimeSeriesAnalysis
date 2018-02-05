baseball=scan()
120
112
105
91
97
98
116
129
133
135
139
139
141
143
146
148
148
148
148
151
153
154
154
145
150
151
151
152
149
148
150
145
143
146
145
145
144
148
146
151
153
153
155
156
155
159
164
167
165
165
167
168
169
168
173
170
171
172
167
173
177
174
172
166
167
166
168
171
170
171
170
171
175
178
182
176
180
185

times = 1:length(baseball)

# perform 1st order differencing
baseball_diff = diff(baseball,lag=1,differences=1)
diff_times = 1:length(baseball_diff)

# plot the differenced values
pdf('plots/baseball_diff_lag1.pdf')
plot(diff_times, baseball_diff)

# test for autocorrelation
Box.test(baseball_diff, lag=15, fitdf=0, type="Ljung")

# plot autocorrelation
pdf('plots/baseball_diff_ACF.pdf')
acf(baseball_diff)