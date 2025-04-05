Test kani::any
  $ bfa-rust exec-main any.rs --clean
  Done. - Ran 5 branches
  PC: 
    (V|7| == 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (0 < V|20|) /\ (0 <= V|20|) /\ (V|20| != 0) /\ (V|20| <= 127) /\
    (-128 <= V|20|) /\ (V|19| != 0) /\ Distinct(V|8-19|) /\ (V|18| != 0) /\
    Distinct(V|8-18|) /\ (V|17| != 0) /\ Distinct(V|8-17|) /\ (V|16| != 0) /\
    Distinct(V|8-16|) /\ (V|15| != 0) /\ Distinct(V|8-15|) /\ (V|14| != 0) /\
    Distinct(V|8-14|) /\ (V|13| != 0) /\ Distinct(V|8-13|) /\ (V|12| != 0) /\
    Distinct(V|8-12|) /\ (V|11| != 0) /\ Distinct(V|8-11|) /\ (V|10| != 0) /\
    Distinct(V|8-10|) /\ (V|9| != 0) /\ Distinct(V|8-9|) /\ (V|8| != 0) /\
    (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|20| < 0) /\ (V|20| != 0) /\ (V|20| <= 127) /\ (-128 <= V|20|) /\
    (V|19| != 0) /\ Distinct(V|8-19|) /\ (V|18| != 0) /\ Distinct(V|8-18|) /\
    (V|17| != 0) /\ Distinct(V|8-17|) /\ (V|16| != 0) /\ Distinct(V|8-16|) /\
    (V|15| != 0) /\ Distinct(V|8-15|) /\ (V|14| != 0) /\ Distinct(V|8-14|) /\
    (V|13| != 0) /\ Distinct(V|8-13|) /\ (V|12| != 0) /\ Distinct(V|8-12|) /\
    (V|11| != 0) /\ Distinct(V|8-11|) /\ (V|10| != 0) /\ Distinct(V|8-10|) /\
    (V|9| != 0) /\ Distinct(V|8-9|) /\ (V|8| != 0) /\ (V|7| != 0) /\
    (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\ Distinct(V|0-6|) /\
    (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\ Distinct(V|0-4|) /\
    (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\ Distinct(V|0-2|) /\
    (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|20| == 0) /\ (V|20| <= 127) /\ (-128 <= V|20|) /\ (V|19| != 0) /\
    Distinct(V|8-19|) /\ (V|18| != 0) /\ Distinct(V|8-18|) /\ (V|17| != 0) /\
    Distinct(V|8-17|) /\ (V|16| != 0) /\ Distinct(V|8-16|) /\ (V|15| != 0) /\
    Distinct(V|8-15|) /\ (V|14| != 0) /\ Distinct(V|8-14|) /\ (V|13| != 0) /\
    Distinct(V|8-13|) /\ (V|12| != 0) /\ Distinct(V|8-12|) /\ (V|11| != 0) /\
    Distinct(V|8-11|) /\ (V|10| != 0) /\ Distinct(V|8-10|) /\ (V|9| != 0) /\
    Distinct(V|8-9|) /\ (V|8| != 0) /\ (V|7| != 0) /\ (V|7| <= 1) /\
    (0 <= V|7|) /\ (V|6| != 0) /\ Distinct(V|0-6|) /\ (V|5| != 0) /\
    Distinct(V|0-5|) /\ (V|4| != 0) /\ Distinct(V|0-4|) /\ (V|3| != 0) /\
    Distinct(V|0-3|) /\ (V|2| != 0) /\ Distinct(V|0-2|) /\ (V|1| != 0) /\
    Distinct(V|0-1|) /\ (V|0| != 0)

Test kani::assume
  $ bfa-rust exec-main assume.rs --clean
  Done. - Ran 2 branches
  PC: 
    (V|5| != 0) /\ (V|5| <= 1) /\ (0 <= V|5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    ((11 / V|13|) <= 2147483647) /\ (-2147483648 <= (11 / V|13|)) /\
    (V|13| != 0) /\ (V|13| <= 2147483647) /\ (-2147483648 <= V|13|) /\
    (V|12| != 0) /\ Distinct(V|6-12|) /\ (V|11| != 0) /\ Distinct(V|6-11|) /\
    (V|10| != 0) /\ Distinct(V|6-10|) /\ (V|9| != 0) /\ Distinct(V|6-9|) /\
    (V|8| != 0) /\ Distinct(V|6-8|) /\ (V|7| != 0) /\ Distinct(V|6-7|) /\
    (V|6| != 0) /\ (V|5| != 0) /\ (V|5| <= 1) /\ (0 <= V|5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)

Test valid values
  $ bfa-rust exec-main should_panic.rs --clean
  Done. - Ran 1 branches
  PC: 
    (V|0| != 0)
