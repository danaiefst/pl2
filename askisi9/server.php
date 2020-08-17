<?php
  function linear_seive($N)
  {
    global $isprime, $MAX_SIZE, $SPF, $prime;
    $isprime[0] = $isprime[1] = false;

    for ($i = 2; $i < $N; $i++)
    {
        if ($isprime[$i])
        {
          array_push($prime, $i);
          $SPF[$i] = $i;
        }
        $j = 0;
        while ($j < count($prime) && $i * $prime[$j] < $N && $prime[$j] <= $SPF[$i]) {
            $isprime[$i * $prime[$j]] = false;
            $SPF[$i * $prime[$j]] = $prime[$j];
            $j += 1;
        }
    }
  }

  function power($a, $n, $p)
  {
    $res = 1;
    $a = $a % $p;
    while ($n > 0)
    {
        if ($n & 1)
          $res = ($res * $a) % $p;
        $n = $n >> 1; // n = n/2
        $a = ($a * $a) % $p;
    }
    return $res;
  }

  function fermat_check($n, $k)
  {
    if ($n <= 1 || $n == 4) return false;
    if ($n <= 3) return true;

    while ($k > 0)
    {
      $a = 2 + rand() % ($n - 4);
      if (power($a, $n-1, $n) != 1) return false;
      $k--;
    }
    return true;
  }

  function check_if_div($p) {
    foreach ($_SESSION['prime'] as $div) {
      if ($div < $p && $p % $div == 0) return false;
    }
    return true;
  }

  function find_prime($n, $k, $extra) {
    global $MAX_N;
    $acc = $n + 1 + $extra;
    while ($acc < $MAX_N) {
      if (fermat_check($acc, 7) && check_if_div($acc)) {
        $cm = comb_mod($n, $k, $acc);
        if ($cm != 0) {
          $_SESSION['combmod'] = $cm;
          return $acc;
        }
      }
      $acc++;
    }
    return null;
  }

  function factorial($start, $end, $mod) {
    $acc = 1;
    for ($i = $start; $i <= $end; $i++) {
      $acc = ($acc * $i) % $mod;
    }
    return $acc;
  }

  function comb_mod($n, $k, $acc) {
    $max = $k;
    $min = $n - $k;
    if ($k < $n - $k) {
      $max = $n - $k;
      $min = $k;
    }
    $fact_nom = factorial($max + 1, $n, $acc);
    $fact_denom = factorial(2, $min, $acc);
    $inv_denom = find_inv($fact_denom, $acc);
    $ret = ($fact_nom * $inv_denom) % $acc;
    if ($ret < 0) $ret += $acc;
    return $ret;
  }

  function find_inv($a, $p) {
      $s = 0;
      $old_s = 1;
      $r = $p;
      $old_r = $a;
      while ($r != 0) {
          $quotient = intdiv($old_r, $r);
          $temp = $old_r;
          $old_r = $r;
          $r = $temp - $quotient * $r;
          $temp = $old_s;
          $old_s = $s;
          $s = $temp - $quotient * $s;
      }
      return $old_s;
  }

  function pretty_print($mis) {
    $f = new NumberFormatter("en", NumberFormatter::SPELLOUT);
    if ($mis == 1) {
        return $f->format($mis)." mistake.";
    }
    return $f->format($mis)." mistakes.";
  }

  session_start();
  $MAX_SIZE = 31623;
  $MAX_N = 1000000000;
  // floor(sqrt(10 ** 9)) + 1
  if ($_POST['restart']) {
    session_destroy();
    header("Refresh:0");
  }
  if (isset($_SESSION['time'])) {
    if($_POST['again']) {
      $submit = 1;
      $n = $_SESSION['n'];
      $k = $_SESSION['k'];
      $p = $_SESSION['p'];
    }
    if($_POST['next']) {
      $submit = 1;
      $_SESSION['question']++;
      if($_SESSION['question'] == 2) {
        $_SESSION['min_n'] = $_SESSION['max_n'];
        $_SESSION['max_n'] = 50;
      }
      elseif($_SESSION['question'] == 3) {
        $_SESSION['min_n'] = $_SESSION['max_n'];
        $_SESSION['max_n'] = 100;
      }
      elseif($_SESSION['question'] == 10) {
        $_SESSION['min_n'] = $_SESSION['max_n'];
        $_SESSION['max_n'] = 999999936; //so that there always exists a prime greater than n (999999937)
      }
      else {
        $_SESSION['min_n'] = $_SESSION['max_n'];
        $_SESSION['max_n'] *= 10;
      }
      $n = rand($_SESSION['min_n'], $_SESSION['max_n']);
      $k = rand($_SESSION['min_n'], $n);
      if($_SESSION['question'] == 10) {
          $p = find_prime($n, $k, 0);
      }
      else {
        $r = rand($_SESSION['min_n'], $_SESSION['max_n']);
        $p = find_prime($n, $k, $r);
      }
      $_SESSION['n'] = $n;
      $_SESSION['k'] = $k;
      $_SESSION['p'] = $p;
    }
    if($_POST["submit"]) {
      if ($_SESSION['combmod'] == $_POST["answer"]) {
        $submit = 0;
        $n = $_SESSION['n'];
        $k = $_SESSION['k'];
        $p = $_SESSION['p'];
        if ($_SESSION['question'] == 10) {
          $submit = 2;
        }
      }
      else {
        $submit = -1;
        $_SESSION['mistakes']++;
        $n = $_SESSION['n'];
        $k = $_SESSION['k'];
        $p = $_SESSION['p'];
      }
    }
  }
  else {
    $isprime = array_fill(0, $MAX_SIZE, true);
    $prime = array();
    $SPF = array_fill(0, $MAX_SIZE, 0);
    linear_seive($MAX_SIZE);
    $_SESSION['prime'] = $prime;
    $_SESSION['mistakes'] = 0;
    $_SESSION['max_n'] = 10;
    $_SESSION['min_n'] = 0;
    $_SESSION['time'] = time();
    $_SESSION['question'] = 1;
    $submit = 1;
    $n = rand($_SESSION['min_n'], $_SESSION['max_n']);
    $k = rand($_SESSION['min_n'], $n);
    $p = find_prime($n, $k, rand(0, 10));
    $_SESSION['n'] = $n;
    $_SESSION['k'] = $k;
    $_SESSION['p'] = $p;
  }
?>


<html xmlns="http://www.w3.org/1999/xhtml"><head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Count combinations in modular arithmetic!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.question { color: #FFCC33; }
.emph     { color: #99ee99; }
.alert    { color: #ee77aa; }

.right {
  color: #33FF66;
  font-weight: bold;
}
.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link    { color: #CCFFFF; }
a:visited { color: #CCFFFF; }

input {
  background-color: #eeee66;
  color: #333399;
}

code {
  font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
  color: #eeee99;
  font-size: 120%;
}

span.removed {
  color: #ff9977;
  text-decoration: underline red;
}

code.block {
  background-color: #66eeee;
  color: #993333;
  overflow-wrap: break-word;
  display: block;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
  line-height: 1em;
  margin-top: 0.25em;
  margin-bottom: 0.25em;
}

input.box {
  overflow-wrap: break-word;
  font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
  font-size: 120%;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
}

input.button {
  font-size: 120%;
  background-color: #99ee99;
  color: #333399;
  border: 1px solid black;
  padding: 8px;
}

-->
</style>
</head>
<body>
<h1>Count combinations in modular arithmetic!</h1>
<p>In how many ways can we choose <span class="emph">K</span> elements out
  of a set of <span class="emph">N</span> elements, if their order is not
  important?  Obviously, the answer is <span class="emph">C(N, K)</span>,
  in other words,
  a <a href="https://en.wikipedia.org/wiki/Binomial_coefficient">binomial
  coefficient</a>.
</p>
<p>This number may be very large and, for this reason, we want to calculate
  the remainder (modulo) of its division by a large prime number
  <span class="emph">P</span>.
</p><p>
</p><p>Assume that <span class="emph">0 ≤ K ≤ N &lt; P ≤ 10<sup>9</sup></span>
  and that <span class="emph">P</span> is a prime number.</p>
<blockquote>
  <p>For example, if <span class="emph">N = 42</span>,
    <span class="emph">K = 17</span> and
    <span class="emph">P = 690419</span>,
    the result is <span class="emph">188587</span>.
  </p>
  <p>In fact, <span class="emph">C(42, 17) = 254661927156 =
    368851 × 690419 + 188587</span>.
  </p>
</blockquote>

<hr>

<p><span class="question" id="question">Question <?= $_SESSION['question'] ?></span>
  </p><p>What is the value of
  <span class="emph">C(<span id="N"><?= $n ?></span>,
    <span id="K"><?= $k ?></span>) modulo
    <span id="P"><?= $p ?></span></span>?
</p>

<?php
  if ($submit == 1) {
?><form action="server.php" id="f" name="f" method="POST">
    <table cellspacing="3" border="0">
      <tbody><tr>
        <td><input type="text" class="box" name="answer" id="answer" autofocus=""></td>
        <td><input type="submit" class="button" name="submit" id="submit" value="Submit!"></td>
       </tr></tbody>
    </table>
  </form> <?php
  }
  elseif ($submit == -1) {
?><p class="wrong">Wrong!  Try again...  :-(</p>
  <form action="server.php" id="r" name="r" method="POST">
    <input class="button" type="submit" name="again" id="again" value="Continue!" autofocus="">
  </form><?php
  }
  elseif ($submit == 0) {
?><p class="right">Right!  :-)</p>
  <form action="server.php" id="r" name="r" method="POST">
    <input class="button" type="submit" name="next" id="again" value="Continue!" autofocus="">
  </form><?php
  }
  else {
?><p class="right">Right!  :-)</p>
  <p><span class="congratulations">Congratulations!</span>
    You answered all questions!
  </p>
  <p>It took you <?= time() - $_SESSION['time'] ?> seconds
    and you made <?php echo (pretty_print($_SESSION['mistakes'])); ?>
  </p>
  <form action="server.php" id="r" name="r" method="post">
    <input class="button" type="submit" name="restart" id="again" value="Play again!" autofocus="">
  </form><?php
  }
?>

</body>
</html>
