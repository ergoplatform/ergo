# Performance And Costing Analysis of Ergo Node

## Introduction
This notebook uses `Metrics.sqlite` database collected during full node syncronization.
See `README.md` for detail of database schema.



## Setup
If necessary uncomment and run the following commands to setup necessary packages.



```python
#!pip install ipython-sql
#!pip install jupyter_contrib_nbextensions
#!jupyter contrib nbextension install --user
!jupyter nbextension enable python-markdown / main

```

    Please specify one nbextension/package at a time



```python
# open sqlite connection to perform queries
import pandas as pd
import sqlite3
conn = sqlite3.connect("../Metrics.sqlite")

```

## v5.0 Validation
- make sure validateTxStateful recorded for each transaction of the block (should be empty)


```python
pd.read_sql_query(f"""
    -- invalid tx_num
    select b.blockId, t.tx_count as c, b.tx_num as n
    from (select blockId, count(*) as tx_count
          from validateTxStateful
          group by blockId) as t
             join applyTransactions as b on b.blockId = t.blockId
    where c != n;
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>blockId</th>
      <th>c</th>
      <th>n</th>
    </tr>
  </thead>
  <tbody>
  </tbody>
</table>
</div>



- make sure recored block cost = sum of recorded tx costs (should be empty)


```python
pd.read_sql_query(f"""
    -- invalid tx cost
    select b.blockId, t.sum_costs as sum_tx_costs, b.cost as block_cost
    from (select blockId, sum(cost) as sum_costs
          from validateTxStateful
          group by blockId) as t
             join applyTransactions as b on b.blockId = t.blockId
    where sum_tx_costs != block_cost;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>blockId</th>
      <th>sum_tx_costs</th>
      <th>block_cost</th>
    </tr>
  </thead>
  <tbody>
  </tbody>
</table>
</div>



- now many transactions have negative cost (should be empty)


```python
pd.read_sql_query(f"""
-- count tx with negative cost
select count(*)
from validateTxStateful
where cost < 0;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
    </tr>
  </tbody>
</table>
</div>



## v5.0 Analysis
### Block Validation Time Analysis
In this section we compare script validation time against block validation time.

#### Sizes of the recorded tables


```python
pd.read_sql_query(f"""
    select * from
    (select count(*) as appendFullBlock
    from appendFullBlock),
    (select count(*) as applyTransactions
    from applyTransactions),
    (select count(*) as createUtxoState
    from createUtxoState),
    (select count(*) as validateTxStateful
    from validateTxStateful),
    (select count(*) as verifyScript
    from verifyScript)
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>appendFullBlock</th>
      <th>applyTransactions</th>
      <th>createUtxoState</th>
      <th>validateTxStateful</th>
      <th>verifyScript</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>488100</td>
      <td>488100</td>
      <td>488100</td>
      <td>1321800</td>
      <td>3823200</td>
    </tr>
  </tbody>
</table>
</div>



#### Comparing Stages of Block Validation
First we look at how much time of the total block validation is spent in applyTransactions.
We group and count blocks by (total time / `applyTransactions` time) ratio.
We see that `applyTransactions` is < 50% for roughly 80% of the blocks.
For more ~40% of the blocks:
1) the ratio is above 3, which means script validation is < 33%
2) further analysis shows that time is spent in creating UtxoState (after applyTransaction)
**Thus, creating UtxoState after application of transactions requires profiling and optimization.**


```python
pd.read_sql_query(f"""
select time_us / t2_us as time_ratio, count(*) as block_count
from (select b1.blockId,
             b1.height,
             b1.tx_num,
             b2.cost,
             b1.time / 1000                       as t1_us,
             b2.time / 1000                       as t2_us,
             b3.time / 1000                       as t3_us,
             (b1.time + b2.time + b3.time) / 1000 as time_us
      from appendFullBlock as b1
               join applyTransactions as b2 on b1.blockId = b2.blockId
               join createUtxoState b3 on b1.blockId = b3.blockId)
group by time_ratio
order by time_ratio;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>block_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>70293</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>155027</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>207666</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>21667</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>20585</td>
    </tr>
    <tr>
      <th>5</th>
      <td>6</td>
      <td>5678</td>
    </tr>
    <tr>
      <th>6</th>
      <td>7</td>
      <td>2472</td>
    </tr>
    <tr>
      <th>7</th>
      <td>8</td>
      <td>1444</td>
    </tr>
    <tr>
      <th>8</th>
      <td>9</td>
      <td>1020</td>
    </tr>
    <tr>
      <th>9</th>
      <td>10</td>
      <td>666</td>
    </tr>
    <tr>
      <th>10</th>
      <td>11</td>
      <td>437</td>
    </tr>
    <tr>
      <th>11</th>
      <td>12</td>
      <td>283</td>
    </tr>
    <tr>
      <th>12</th>
      <td>13</td>
      <td>186</td>
    </tr>
    <tr>
      <th>13</th>
      <td>14</td>
      <td>106</td>
    </tr>
    <tr>
      <th>14</th>
      <td>15</td>
      <td>66</td>
    </tr>
    <tr>
      <th>15</th>
      <td>16</td>
      <td>73</td>
    </tr>
    <tr>
      <th>16</th>
      <td>17</td>
      <td>57</td>
    </tr>
    <tr>
      <th>17</th>
      <td>18</td>
      <td>41</td>
    </tr>
    <tr>
      <th>18</th>
      <td>19</td>
      <td>38</td>
    </tr>
    <tr>
      <th>19</th>
      <td>20</td>
      <td>34</td>
    </tr>
    <tr>
      <th>20</th>
      <td>21</td>
      <td>26</td>
    </tr>
    <tr>
      <th>21</th>
      <td>22</td>
      <td>37</td>
    </tr>
    <tr>
      <th>22</th>
      <td>23</td>
      <td>35</td>
    </tr>
    <tr>
      <th>23</th>
      <td>24</td>
      <td>25</td>
    </tr>
    <tr>
      <th>24</th>
      <td>25</td>
      <td>24</td>
    </tr>
    <tr>
      <th>25</th>
      <td>26</td>
      <td>18</td>
    </tr>
    <tr>
      <th>26</th>
      <td>27</td>
      <td>10</td>
    </tr>
    <tr>
      <th>27</th>
      <td>28</td>
      <td>11</td>
    </tr>
    <tr>
      <th>28</th>
      <td>29</td>
      <td>16</td>
    </tr>
    <tr>
      <th>29</th>
      <td>30</td>
      <td>4</td>
    </tr>
    <tr>
      <th>30</th>
      <td>31</td>
      <td>5</td>
    </tr>
    <tr>
      <th>31</th>
      <td>32</td>
      <td>5</td>
    </tr>
    <tr>
      <th>32</th>
      <td>33</td>
      <td>9</td>
    </tr>
    <tr>
      <th>33</th>
      <td>34</td>
      <td>7</td>
    </tr>
    <tr>
      <th>34</th>
      <td>35</td>
      <td>1</td>
    </tr>
    <tr>
      <th>35</th>
      <td>36</td>
      <td>3</td>
    </tr>
    <tr>
      <th>36</th>
      <td>37</td>
      <td>3</td>
    </tr>
    <tr>
      <th>37</th>
      <td>38</td>
      <td>2</td>
    </tr>
    <tr>
      <th>38</th>
      <td>39</td>
      <td>1</td>
    </tr>
    <tr>
      <th>39</th>
      <td>40</td>
      <td>1</td>
    </tr>
    <tr>
      <th>40</th>
      <td>42</td>
      <td>3</td>
    </tr>
    <tr>
      <th>41</th>
      <td>44</td>
      <td>1</td>
    </tr>
    <tr>
      <th>42</th>
      <td>47</td>
      <td>2</td>
    </tr>
    <tr>
      <th>43</th>
      <td>49</td>
      <td>1</td>
    </tr>
    <tr>
      <th>44</th>
      <td>51</td>
      <td>1</td>
    </tr>
    <tr>
      <th>45</th>
      <td>52</td>
      <td>2</td>
    </tr>
    <tr>
      <th>46</th>
      <td>132</td>
      <td>1</td>
    </tr>
    <tr>
      <th>47</th>
      <td>139</td>
      <td>1</td>
    </tr>
    <tr>
      <th>48</th>
      <td>166</td>
      <td>1</td>
    </tr>
    <tr>
      <th>49</th>
      <td>208</td>
      <td>1</td>
    </tr>
    <tr>
      <th>50</th>
      <td>215</td>
      <td>1</td>
    </tr>
    <tr>
      <th>51</th>
      <td>229</td>
      <td>1</td>
    </tr>
    <tr>
      <th>52</th>
      <td>292</td>
      <td>1</td>
    </tr>
    <tr>
      <th>53</th>
      <td>327</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>



#### Comparing applyTransaction of block with validateTxStateful
Here we further drill down to applyTransactions part of block validation.
Specifically, for each block we compare the time of `UtxoState.applyTransactions` with total time
of `ErgoTransaction.validateStateful` taken for all transactions in the block.
The blocks are grouped by the ratio between times.
We can see that for > 70% blocks the ration is above 2, which suggests that
**`UtxoState.applyTransactions` method need optimizations.**


```python
pd.read_sql_query(f"""
select t.time_ratio / 10 as time_ratio,
       count(*) as block_count,
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.tx_time_us), 0) as avg_tx_time_us,
       round(avg(t.tx_count), 1)   as avg_tx_count
from (select b.blockId,
             tx.tx_count,
             b.time / 1000                        as block_time_us,
             tx.sum_tx_time / 1000            as tx_time_us,
             (b.time - tx.sum_tx_time) / 1000 as time_diff_us,
             b.time * 10 / tx.sum_tx_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_tx_time,
                   count(*)  as tx_count
            from validateTxStateful
            group by blockId) as tx
               join applyTransactions as b on b.blockId = tx.blockId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>block_count</th>
      <th>avg_block_time_us</th>
      <th>avg_tx_time_us</th>
      <th>avg_tx_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>148957</td>
      <td>17974.0</td>
      <td>15156.0</td>
      <td>6.6</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>170380</td>
      <td>1193.0</td>
      <td>425.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>167950</td>
      <td>1151.0</td>
      <td>375.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>256</td>
      <td>3337.0</td>
      <td>749.0</td>
      <td>1.3</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>138</td>
      <td>3965.0</td>
      <td>725.0</td>
      <td>1.3</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>63</th>
      <td>75</td>
      <td>1</td>
      <td>31507.0</td>
      <td>417.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>64</th>
      <td>82</td>
      <td>1</td>
      <td>32388.0</td>
      <td>392.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>65</th>
      <td>83</td>
      <td>1</td>
      <td>33706.0</td>
      <td>404.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>66</th>
      <td>86</td>
      <td>1</td>
      <td>41055.0</td>
      <td>477.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>67</th>
      <td>88</td>
      <td>1</td>
      <td>31910.0</td>
      <td>362.0</td>
      <td>1.0</td>
    </tr>
  </tbody>
</table>
<p>68 rows × 5 columns</p>
</div>



#### Tx/Script validation time ratio
Further down to validation call stack, we observe how much `validateStateful` time larger than
script verification time.
The ratio is computed for each transaction and then the transactions are grouped by integer ratio.
Comparing times in `verifyScript` and `validateTxStateful` metrics.
We see that for most transactions, `verifyScript` is > 50% of `validateStateful`.


```python
pd.read_sql_query(f"""
select t.time_ratio / 10 as time_ratio,
       count(*) as tx_count,
       round(avg(t.tx_time_us), 1) as avg_tx_time_us,
       round(avg(t.script_time_us), 1) as avg_script_time_us,
       round(avg(t.script_count), 1) as avg_script_count
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>tx_count</th>
      <th>avg_tx_time_us</th>
      <th>avg_script_time_us</th>
      <th>avg_script_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>1321331</td>
      <td>1806.3</td>
      <td>1765.3</td>
      <td>2.9</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>154</td>
      <td>7181.1</td>
      <td>2900.2</td>
      <td>6.1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>63</td>
      <td>11364.8</td>
      <td>3407.0</td>
      <td>8.5</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>43</td>
      <td>14648.4</td>
      <td>3341.8</td>
      <td>7.6</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>24</td>
      <td>12482.9</td>
      <td>2229.4</td>
      <td>4.5</td>
    </tr>
    <tr>
      <th>5</th>
      <td>6</td>
      <td>30</td>
      <td>11152.2</td>
      <td>1712.3</td>
      <td>4.7</td>
    </tr>
    <tr>
      <th>6</th>
      <td>7</td>
      <td>24</td>
      <td>22004.2</td>
      <td>3038.2</td>
      <td>6.2</td>
    </tr>
    <tr>
      <th>7</th>
      <td>8</td>
      <td>14</td>
      <td>9936.7</td>
      <td>1192.9</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>8</th>
      <td>9</td>
      <td>7</td>
      <td>18300.3</td>
      <td>1928.6</td>
      <td>5.1</td>
    </tr>
    <tr>
      <th>9</th>
      <td>10</td>
      <td>10</td>
      <td>10140.6</td>
      <td>972.2</td>
      <td>4.3</td>
    </tr>
    <tr>
      <th>10</th>
      <td>11</td>
      <td>7</td>
      <td>12251.3</td>
      <td>1091.3</td>
      <td>4.0</td>
    </tr>
    <tr>
      <th>11</th>
      <td>12</td>
      <td>9</td>
      <td>66606.8</td>
      <td>5249.7</td>
      <td>12.3</td>
    </tr>
    <tr>
      <th>12</th>
      <td>13</td>
      <td>6</td>
      <td>55290.5</td>
      <td>4154.7</td>
      <td>12.3</td>
    </tr>
    <tr>
      <th>13</th>
      <td>14</td>
      <td>2</td>
      <td>25408.5</td>
      <td>1772.5</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>14</th>
      <td>15</td>
      <td>8</td>
      <td>21613.8</td>
      <td>1401.9</td>
      <td>2.9</td>
    </tr>
    <tr>
      <th>15</th>
      <td>16</td>
      <td>3</td>
      <td>24489.7</td>
      <td>1463.7</td>
      <td>3.7</td>
    </tr>
    <tr>
      <th>16</th>
      <td>17</td>
      <td>4</td>
      <td>9498.5</td>
      <td>547.8</td>
      <td>1.3</td>
    </tr>
    <tr>
      <th>17</th>
      <td>18</td>
      <td>5</td>
      <td>11864.2</td>
      <td>646.8</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>18</th>
      <td>19</td>
      <td>5</td>
      <td>14664.4</td>
      <td>739.6</td>
      <td>1.8</td>
    </tr>
    <tr>
      <th>19</th>
      <td>20</td>
      <td>3</td>
      <td>26642.3</td>
      <td>1316.7</td>
      <td>3.3</td>
    </tr>
    <tr>
      <th>20</th>
      <td>21</td>
      <td>5</td>
      <td>10858.8</td>
      <td>506.4</td>
      <td>1.6</td>
    </tr>
    <tr>
      <th>21</th>
      <td>22</td>
      <td>5</td>
      <td>15152.0</td>
      <td>675.6</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>22</th>
      <td>23</td>
      <td>5</td>
      <td>12719.6</td>
      <td>543.8</td>
      <td>1.4</td>
    </tr>
    <tr>
      <th>23</th>
      <td>24</td>
      <td>5</td>
      <td>8051.6</td>
      <td>323.2</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>24</th>
      <td>26</td>
      <td>1</td>
      <td>10179.0</td>
      <td>377.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>25</th>
      <td>34</td>
      <td>2</td>
      <td>7845.5</td>
      <td>228.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>26</th>
      <td>35</td>
      <td>2</td>
      <td>27151.5</td>
      <td>756.5</td>
      <td>4.5</td>
    </tr>
    <tr>
      <th>27</th>
      <td>37</td>
      <td>1</td>
      <td>17464.0</td>
      <td>464.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>28</th>
      <td>43</td>
      <td>1</td>
      <td>42704.0</td>
      <td>972.0</td>
      <td>6.0</td>
    </tr>
    <tr>
      <th>29</th>
      <td>44</td>
      <td>1</td>
      <td>6739.0</td>
      <td>152.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>30</th>
      <td>48</td>
      <td>1</td>
      <td>22134.0</td>
      <td>460.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>31</th>
      <td>56</td>
      <td>1</td>
      <td>21106.0</td>
      <td>373.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>32</th>
      <td>59</td>
      <td>1</td>
      <td>29897.0</td>
      <td>505.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>33</th>
      <td>60</td>
      <td>1</td>
      <td>9782.0</td>
      <td>160.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>34</th>
      <td>61</td>
      <td>1</td>
      <td>17317.0</td>
      <td>279.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>35</th>
      <td>69</td>
      <td>1</td>
      <td>25691.0</td>
      <td>370.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>36</th>
      <td>72</td>
      <td>1</td>
      <td>37181.0</td>
      <td>509.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>37</th>
      <td>75</td>
      <td>1</td>
      <td>36907.0</td>
      <td>486.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>38</th>
      <td>82</td>
      <td>1</td>
      <td>116033.0</td>
      <td>1410.0</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>39</th>
      <td>96</td>
      <td>1</td>
      <td>30594.0</td>
      <td>317.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>40</th>
      <td>107</td>
      <td>1</td>
      <td>34966.0</td>
      <td>324.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>41</th>
      <td>129</td>
      <td>1</td>
      <td>239188.0</td>
      <td>1853.0</td>
      <td>2.0</td>
    </tr>
  </tbody>
</table>
</div>



We need to further drill down inside the mose populated group.
We build a detailed grouping of the transactions where the ratio <= 1.9,
comparing times in `verifyScript` and `validateTxStateful` metrics.


```python
pd.read_sql_query(f"""
select round(t.time_ratio * 0.1, 1) as time_ratio,
       count(*) as tx_count,
       round(avg(t.tx_time_us), 1) as avg_tx_time_us,
       round(avg(t.script_time_us), 1) as avg_script_time_us,
       round(avg(t.script_count), 1) as avg_script_count
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>tx_count</th>
      <th>avg_tx_time_us</th>
      <th>avg_script_time_us</th>
      <th>avg_script_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1.0</td>
      <td>1211507</td>
      <td>1934.7</td>
      <td>1895.0</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1.1</td>
      <td>101236</td>
      <td>337.2</td>
      <td>299.3</td>
      <td>1.2</td>
    </tr>
    <tr>
      <th>2</th>
      <td>1.2</td>
      <td>6944</td>
      <td>736.0</td>
      <td>599.2</td>
      <td>1.6</td>
    </tr>
    <tr>
      <th>3</th>
      <td>1.3</td>
      <td>498</td>
      <td>2257.4</td>
      <td>1673.0</td>
      <td>4.4</td>
    </tr>
    <tr>
      <th>4</th>
      <td>1.4</td>
      <td>869</td>
      <td>970.4</td>
      <td>671.6</td>
      <td>2.2</td>
    </tr>
    <tr>
      <th>5</th>
      <td>1.5</td>
      <td>102</td>
      <td>4768.1</td>
      <td>3073.3</td>
      <td>6.2</td>
    </tr>
    <tr>
      <th>6</th>
      <td>1.6</td>
      <td>51</td>
      <td>5595.5</td>
      <td>3370.3</td>
      <td>9.6</td>
    </tr>
    <tr>
      <th>7</th>
      <td>1.7</td>
      <td>44</td>
      <td>8933.2</td>
      <td>5113.4</td>
      <td>10.8</td>
    </tr>
    <tr>
      <th>8</th>
      <td>1.8</td>
      <td>42</td>
      <td>6566.0</td>
      <td>3518.1</td>
      <td>8.1</td>
    </tr>
    <tr>
      <th>9</th>
      <td>1.9</td>
      <td>38</td>
      <td>4534.5</td>
      <td>2315.4</td>
      <td>3.6</td>
    </tr>
  </tbody>
</table>
</div>



Count transactions where script validation <= 80% of `validateStatefull`.
This DOESN'T show big potential for optimizing `validateStateful` outside script evaluation.
Comparing times in `verifyScript` and `validateTxStateful` metrics.


```python
pd.read_sql_query(f"""
select count(*) as tx_count
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
where t.time_ratio >= 12;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>tx_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>9049</td>
    </tr>
  </tbody>
</table>
</div>



Conclusions:
- Creating UtxoState after application of transactions requires profiling and optimization.
- `UtxoState.applyTransactions` method need optimizations.
- In most of the cases the time to validate transaction (`validateStateful` method) is dominated by
the script validation time
- There is a few transactions where script validation not greater than 80% of tx validation time.
- The results suggest that `validateStateful` doesn't require optimizations, or at least it can
be done after the other parts of block validation had been optimized.

### Block Validation Cost Analysis
In this section we compare script validation costs against transaction and block validation costs.

## v5.0 vs v4.0 Cross Analysis

### Cross Version validation
- Make sure recorded data for v5 and v4 have the same `height` and `tx_num` for all blockIds



```python
checks = dict(
    appendFullBlockOk=pd.read_sql_query(f"""
        -- validate appendFullBlock tables
        select *
        from appendFullBlock a1
                 join appendFullBlock4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0,
    applyTransactionsOk=pd.read_sql_query(f"""
        select *
        from applyTransactions a1
                 join applyTransactions4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0,
    createUtxoStateOk=pd.read_sql_query(f"""
        select *
        from createUtxoState a1
                 join createUtxoState4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0
)
if not (checks.get("appendFullBlockOk") and checks.get("applyTransactionsOk") and checks.get(
        "createUtxoStateOk")):
    ok = checks
else:
    ok = "ok"
print(checks)

```

    {'appendFullBlockOk': True, 'applyTransactionsOk': True, 'createUtxoStateOk': True}


### Block Validation Times
First count recorded block validations of v4 and v5 and how much of them can be
joined. This is also validation check, because `common_rows` should be equal to the minimal count.


```python
pd.read_sql_query(f"""
select *, count_rows5 - count_rows4 as v5_v4_rows_diff
from (select count(*) as count_rows5 from applyTransactions),
     (select count(*) as count_rows4 from applyTransactions4),
     (select count(*) as common_rows
      from applyTransactions as t5 join applyTransactions4 t4 on t5.blockId = t4.blockId);
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>count_rows5</th>
      <th>count_rows4</th>
      <th>common_rows</th>
      <th>v5_v4_rows_diff</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>488100</td>
      <td>473000</td>
      <td>473000</td>
      <td>15100</td>
    </tr>
  </tbody>
</table>
</div>



The new v5 script interpreter is expected to perform faster comparing to v4.
The following shows the distribution of the blocks across ranges of speedup. We see that for the
most of the blocks the speedup is in a range from 1 (no speedup) to 2 (twice faster).


```python
pd.read_sql_query(f"""
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
group by speedup order by speedup;
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>8048</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>449950</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>9866</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1950</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>962</td>
    </tr>
    <tr>
      <th>5</th>
      <td>5</td>
      <td>734</td>
    </tr>
    <tr>
      <th>6</th>
      <td>6</td>
      <td>547</td>
    </tr>
    <tr>
      <th>7</th>
      <td>7</td>
      <td>206</td>
    </tr>
    <tr>
      <th>8</th>
      <td>8</td>
      <td>191</td>
    </tr>
    <tr>
      <th>9</th>
      <td>9</td>
      <td>132</td>
    </tr>
    <tr>
      <th>10</th>
      <td>10</td>
      <td>103</td>
    </tr>
    <tr>
      <th>11</th>
      <td>11</td>
      <td>69</td>
    </tr>
    <tr>
      <th>12</th>
      <td>12</td>
      <td>37</td>
    </tr>
    <tr>
      <th>13</th>
      <td>13</td>
      <td>28</td>
    </tr>
    <tr>
      <th>14</th>
      <td>14</td>
      <td>25</td>
    </tr>
    <tr>
      <th>15</th>
      <td>15</td>
      <td>9</td>
    </tr>
    <tr>
      <th>16</th>
      <td>16</td>
      <td>16</td>
    </tr>
    <tr>
      <th>17</th>
      <td>17</td>
      <td>7</td>
    </tr>
    <tr>
      <th>18</th>
      <td>18</td>
      <td>12</td>
    </tr>
    <tr>
      <th>19</th>
      <td>19</td>
      <td>7</td>
    </tr>
    <tr>
      <th>20</th>
      <td>20</td>
      <td>8</td>
    </tr>
    <tr>
      <th>21</th>
      <td>21</td>
      <td>8</td>
    </tr>
    <tr>
      <th>22</th>
      <td>22</td>
      <td>5</td>
    </tr>
    <tr>
      <th>23</th>
      <td>23</td>
      <td>3</td>
    </tr>
    <tr>
      <th>24</th>
      <td>24</td>
      <td>5</td>
    </tr>
    <tr>
      <th>25</th>
      <td>25</td>
      <td>6</td>
    </tr>
    <tr>
      <th>26</th>
      <td>26</td>
      <td>7</td>
    </tr>
    <tr>
      <th>27</th>
      <td>27</td>
      <td>6</td>
    </tr>
    <tr>
      <th>28</th>
      <td>28</td>
      <td>4</td>
    </tr>
    <tr>
      <th>29</th>
      <td>29</td>
      <td>7</td>
    </tr>
    <tr>
      <th>30</th>
      <td>30</td>
      <td>2</td>
    </tr>
    <tr>
      <th>31</th>
      <td>31</td>
      <td>3</td>
    </tr>
    <tr>
      <th>32</th>
      <td>32</td>
      <td>4</td>
    </tr>
    <tr>
      <th>33</th>
      <td>33</td>
      <td>4</td>
    </tr>
    <tr>
      <th>34</th>
      <td>34</td>
      <td>11</td>
    </tr>
    <tr>
      <th>35</th>
      <td>35</td>
      <td>3</td>
    </tr>
    <tr>
      <th>36</th>
      <td>37</td>
      <td>3</td>
    </tr>
    <tr>
      <th>37</th>
      <td>38</td>
      <td>1</td>
    </tr>
    <tr>
      <th>38</th>
      <td>39</td>
      <td>1</td>
    </tr>
    <tr>
      <th>39</th>
      <td>40</td>
      <td>2</td>
    </tr>
    <tr>
      <th>40</th>
      <td>41</td>
      <td>1</td>
    </tr>
    <tr>
      <th>41</th>
      <td>43</td>
      <td>1</td>
    </tr>
    <tr>
      <th>42</th>
      <td>44</td>
      <td>1</td>
    </tr>
    <tr>
      <th>43</th>
      <td>48</td>
      <td>1</td>
    </tr>
    <tr>
      <th>44</th>
      <td>49</td>
      <td>1</td>
    </tr>
    <tr>
      <th>45</th>
      <td>54</td>
      <td>1</td>
    </tr>
    <tr>
      <th>46</th>
      <td>58</td>
      <td>1</td>
    </tr>
    <tr>
      <th>47</th>
      <td>62</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>



We see outliers, i.e. blocks which executed slower in v5 than in v4. This can be attributed to
measurement fluctuations. Such blocks will be simply filtered out in the following analysis.

We also see a long tail of blocks with higher than 2 speedups.


```python
pd.read_sql_query(f"""
select sum(num_blocks) from (
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where speedup >= 2
group by speedup order by speedup
)
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>sum(num_blocks)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>15002</td>
    </tr>
  </tbody>
</table>
</div>



Let's see how the speedup is spread over ranges of the blockchain.
The block `b` falls in `range` if `range = b.height / 100000`. The blocks of each range are counted,
and the average speedup over the range is computed.


```python
pd.read_sql_query(f"""
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*)                                      as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time / t5.time >= 1
group by range order by range;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>range</th>
      <th>avg_speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>1.26</td>
      <td>97603</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>1.29</td>
      <td>98368</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>1.34</td>
      <td>99011</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1.40</td>
      <td>98192</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>1.45</td>
      <td>71778</td>
    </tr>
  </tbody>
</table>
</div>



We observe that the average speedup increase towards the most recent range, which is expected, because
more and more complex contracts are used on chain over time. The v5 script interpreter is by design
faster on complex contracts.

If we further constrain speedup to be above 1.2, we get the following distribution of blocks.


```python
pd.read_sql_query(f"""
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time * 100 / t5.time * 0.01 >= 1.2
group by range order by range
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>range</th>
      <th>avg_speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>1.34</td>
      <td>60383</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>1.35</td>
      <td>68973</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>1.39</td>
      <td>79345</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1.44</td>
      <td>84885</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>1.52</td>
      <td>60029</td>
    </tr>
  </tbody>
</table>
</div>



Note, this speedups are due to improvements in script evaluation which is further analyzed in the
[next section](#script-validation-time). However, as shown in [Comparing Stages of Block
Validation](#comparing-stages-of-block-validation), for most of the blocks, script evaluation is
less than 50% of the block validation time, so the impact of script speedups is limited by this
factor.


### Script Validation Time
In this section we focus on script evaluation part only (i.e. Interpreter.verify method), which is
measured and recorded in `verifyScript` table.

First count recorded script validations of v4 and v5 and how much of them can be
joined. This is also validation check, because `common_rows` should be equal to the minimal count.


```python
pd.read_sql_query(f"""
select *, total_rows5 - total_rows4 as v5_v4_rows_diff from
(select count(*) as total_rows5
from verifyScript as s),
(select count(*) as total_rows4
from verifyScript4 as s),
(select count(*) as common_rows
from verifyScript as t5
         join verifyScript4 t4
              on t5.blockId = t4.blockId
                  and t5.txId = t4.txId
                  and t5.boxIndex = t4.boxIndex);
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>total_rows5</th>
      <th>total_rows4</th>
      <th>common_rows</th>
      <th>v5_v4_rows_diff</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>3823200</td>
      <td>3537350</td>
      <td>3537350</td>
      <td>285850</td>
    </tr>
  </tbody>
</table>
</div>



  For common recorded script we compute the total time spend in script validation for both v4 and v5
  and compare them. Then compute the expected percent of total script time reduction after switching
  to v5.0.


```python
pd.read_sql_query(f"""
select times.total_time4 / 1000                                   as total_time4_us,
       times.total_time5 / 1000                                   as total_time5_us,
       (times.total_time4 - times.total_time5) / 1000             as total_diff_us,
       round((1 - round(times.total_time5 * 100 / times.total_time4 * 0.01, 1)) * 100, 1) as percent_of_reduction
from (select sum(t5.time) as total_time5,
             sum(t4.time) as total_time4
      from verifyScript as t5
               join verifyScript4 t4
                    on t5.blockId = t4.blockId
                        and t5.txId = t4.txId
                        and t5.boxIndex = t4.boxIndex) as times;
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>total_time4_us</th>
      <th>total_time5_us</th>
      <th>total_diff_us</th>
      <th>percent_of_reduction</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>3175910119</td>
      <td>2207370816</td>
      <td>968539302</td>
      <td>30.0</td>
    </tr>
  </tbody>
</table>
</div>


