<?php
  #checks if a number is prime
  function is_prime($num){
    if ($num<=1) return false;
    if ($num<=3) return true;
    if ($num % 2 == 0 || $num % 3 == 0) return false;
    for ($i=5; $i*$i < $num; $i+=6) {
      if ($num%$i == 0 || $num%($i+2) == 0) return false;
    }
    return true;
  }

  #finds the next prime number, given a number num
  function next_prime ($num){
    if ($num<2) return 2;
    $prime = $num;
    $found = 0;
    while (!$found) {
      $prime++;
      if (is_prime($prime)) $found=1;
    }
    return $prime;
  }

  #takes an array,called args of 4 integers, a,b,x,y.
  #for these numbers we have that a*x + b*y = gcd(a,b)
  #at return we have that args[1] = gcd(a,b), args[2]=x, args[3]=y
  function gcdExtended($args){
    if ($args[0] == 0){
      $args[2] = 0;
      $args[3] = 1;
      return $args;      //return b
    }
    $ret = gcdExtended(create_args($args[1] % $args[0],$args[0],$args[2],$args[3]));
    $args[2] = $ret[3]-floor($args[1] / $args[0]) * $ret[2];
    $args[3] = $ret[2];
    return $args;
  }

  #helper function that takes 4 arguments a,b,x,y and returns an array of those 4
  function create_args($a,$b,$x,$y) {
    $args[0] = $a;
    $args[1] = $b;
    $args[2] = $x;
    $args[3] = $y;
    return $args;
  }

  #takes a number x and a number p and returns the inverse of x
  #for which we have that x*x^-1 = 1 mod p
  function inverse ($x,$p) {
    $ret = gcdExtended(create_args($x,$p,0,0));
    if ($ret[2] < 0) return $ret[2]+$p;           #finds modulo of negative x
    else return $ret[2];
  }

  #returns a * b mod p
  function modMult ($a,$b,$p) {
    return  $a * $b % $p;
  }

  #returns n!/k! mod p (n > k). for k = 1 returns n! mod p
  function modFact ($n,$k,$p) {
    $res = 1;
    while ($n > $k) {
      $res = $res * $n % $p;
      $n--;
    }
    return $res;
  }

  #finds the binomial_coefficient nCk mod p
  #fast and effective
  function binomial_coefficient($n, $k, $p) {
    return modMult(modFact($n,$k,$p),inverse(modFact($n-$k,1,$p),$p),$p);
  }

  function generate(){
    global $max;
    $maxnum = $max[$_SESSION['count']];
    $_SESSION['K'] = rand(0,$maxnum);
    $_SESSION['N'] = rand($_SESSION['K'],$maxnum);
    $_SESSION['P'] = next_prime($_SESSION['N']);
    $_SESSION['answer'] = binomial_coefficient ($_SESSION['N'],$_SESSION['K'],$_SESSION['P']);
    $_SESSION['count']++;
  }


  $max = array(10,30,50,100,1000,10000,100000,10000000,100000000,1000000000);         //array for question's max num
  session_start();

  ?>

<!DOCTYPE html>
<html lang="en" dir="ltr">
<link href="ask9.css" rel="stylesheet" type="text/css" />
  <head>
    <meta charset="utf-8">
    <title>Binomial Coefficient</title>
  </head>
  <?php
    if (!isset($_SESSION['count']) || isset($_SESSION['reset'])) {
      $_SESSION['count'] = 0;
      $_SESSION['wrong'] = 0;
    }
    if (isset($_SESSION['generate']) || $_SESSION['count'] == 0) {
      generate();
    }
    unset($_SESSION['generate']);
    unset($_SESSION['reset']);
?>
  <body>
    <h1>Question <?php echo ($_SESSION['count']) ?></h1>
    <h2>Which is the binomial Coefficient for N =<?php echo $_SESSION['N']; ?>,
        K =<?php echo $_SESSION['K']; ?> mod P =<?php echo $_SESSION['P']; ?>
    </h2>
    <form class="" action="index.php" method="post">
      <table>
        <tr>
          <td>
            <input type="text" class="question" name="answer" id="answer" value="" autofocus>
          </td>
<?php
  echo $_SESSION['answer'];
  if (isset($_SESSION['cheat'])) {
    echo $_SESSION['answer'];
  }
  if (isset($_POST['answer']) && $_POST['answer'] != "") {
    if ($_POST['answer'] == $_SESSION['answer']) {
      printf("<td><span class=\"correct\">CORRECT</span></td>\n");
    }
    else {
      printf("<td><span class=\"wrong\">WRONG</span></td>\n");
      $_SESSION['count']--;
      $_SESSION['wrong']++;
    }
    if ($_SESSION['count'] < 5 ) {        # TO DO IT 1O <--|-|-|-\-|-\-\-\-\-\-\-\-\-\-\
      $_SESSION['generate'] = 1;
      printf("<td width=\"16\">&nbsp;</td>\n");
      printf("<td><input type=\"submit\" class=\"submit\" name=\"continue\"
                         id=\"continue\" value=\"Continue!\" /></td>\n");
    }
    else {
      $_SESSION['reset'] = 1;
      printf("<td><input type=\"submit\" class=\"submit\" name=\"reset\"
                         id=\"reset\" value=\"Play Again!\" /></td>\n");
    }
  }
  else {
    printf("<td><input type=\"submit\" class=\"submit\" name=\"submit\" value=\"Submit!\"></td>");
  }
 ?>
        </tr>
      </table>
<?php
  if (isset($_SESSION['reset'])) {
 ?>
 <p class="win">You Won</p>
 <p class="mistakes">You made <?php echo $_SESSION['wrong'] ?> mistakes</p>
 <?php
  }
  ?>
    </form>
  </body>
</html>