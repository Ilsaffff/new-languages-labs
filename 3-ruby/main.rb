def factorize_quadratic(a, b, c)
    raise "Leading coefficient (a) cannot be zero" if a == 0
  
    product = a * c
  
    m, n = nil, nil
    (1..product.abs).each do |i|
      [i, -i].each do |x|
        y = product / x
        if x * y == product && x + y == b
          m, n = x, y
          break
        end
      end
      break if m && n
    end
  
    return "Cannot factorize: no suitable m, n found" unless m && n
  
    factor1 = gcd(a, m)
    factor2 = gcd(n, c)
  
    factor_a = "#{factor1}x + #{m / factor1}"
    factor_b = "#{factor2}x + #{c / factor2}"
  
    return "(#{factor_a})(#{factor_b})"
  end
  
  def gcd(x, y)
    x, y = x.abs, y.abs
    while y != 0
      x, y = y, x % y
    end
    x
  end
  
  a, b, c = 11, 100, -10 
  
  begin
    result = factorize_quadratic(a, b, c)
    puts "Factorization: #{result}"
  rescue => e
    puts "Error: #{e.message}"
  end