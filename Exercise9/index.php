<?php
  function is_prime($num){
    if ($num<=1) return false;
    if ($num<=3) return true;
    if ($num % 2 == 0 || $num % 3 == 0) return false;
    for ($i=5; $i*$i < $num; $i+=6) {
      if ($num%$i == 0 || $num%($i+2) == 0) return false;
    }
    return true;
  }
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
  function binomial_coefficient($n, $k, $p) {
    $result = 1;
    for ($i=1; $i < $k+1; $i++) {
      $result = $result *($n-$i+1)/$i ;
    }
    return $result;
  }
  $max = array(10,30,50,100,1000,100000,1000000,10000000,1000000000,1000000000);         //array for question's max num
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
          $_SESSION['count']=3;
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
