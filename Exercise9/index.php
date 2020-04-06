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
    return  ($a%$p) * ($b%$p) % $p;
  }

  #returns n! mod p
  function modFact ($n,$p) {
    $res = 1;
    while ($n > 1) {
      $res = $res * $n % $p;
      $n--;
    }
    return $res;
  }

  #finds the binomial_coefficient nCk mod p
  #fast and effective
  function binomial_coefficient($n, $k, $p) {
    return modMult(modMult(modFact($n,$p),inverse(modFact($k,$p),$p),$p),inverse(modFact($n-$k,$p),$p),$p);
  }
  $max = array(10,30,50);         //array for question's max num
  session_start();

  function generate(){
    global $max;
    if ($_SESSION['gen'] == 1) {
      $maxnum = $max[$_SESSION['count']];
      $_SESSION['K'] = rand(0,$maxnum);
      $_SESSION['N'] = rand($_SESSION['K'],$maxnum);
      $_SESSION['P'] = next_prime($_SESSION['N']);
      $_SESSION['correct'] = binomial_coefficient ($_SESSION['N'],$_SESSION['K'],$_SESSION['P']);
      $_SESSION['gen']=0;
    }
  }
  ?>

<!DOCTYPE html>
<html lang="en" dir="ltr">
  <head>
    <meta charset="utf-8">
    <title>Binomial Coefficient</title>
  </head>
  <body>
    <?php $_SESSION['answer']=0;
          $_SESSION['count']=2;
          $_SESSION['gen']=1;
          generate();
    ?>
    <h1>Question <?php echo ($_SESSION['count']+1) ?></h1>
    <h2>Which is the binomial Coefficient for</h2>
    <h3>N=<span id="N"><?php echo $_SESSION['N']; ?></span></h3>
    <h3>K=<span id="K"><?php echo $_SESSION['K']; ?></span></h3>
    <h3>P=<span id="P"><?php echo $_SESSION['P']; ?></span></h3>
    <form class="" action="index.php" method="post">
      <table>
        <tr>
          <td>
            <input type="text" name="answer" id="answer" value="<?php echo $_SESSION['correct']  ?>" autofocus>
          </td>
          <td>
            <input type="submit" name="submit" value="Submit!">
          </td>
        </tr>
      </table>
    </form>
  </body>
</html>
