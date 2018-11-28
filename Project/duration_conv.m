function[duration, convexity] = duration_conv(p, cf, t, r)
    duration = 1/p * sum((cf' .* t') ./ ((1 + r).^(t' + 1)));
    convexity = 1/p * sum((cf' .* t') .* (t'+1)  ./ (1 + r).^(t'+2));

end

