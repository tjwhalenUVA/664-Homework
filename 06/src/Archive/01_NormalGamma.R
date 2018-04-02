### NormalGamma Package

There are two additional values to go along with the number of observations (sample mean, and Sum of Squared Deviations).
All of these values get used next to update the distribution parameters.

```{r}
#Sample Mean
bottom.mean <- sum(pollutants$Bottom) / n
bottom.mean
```
```{r}
#Sum of Squared Deviations
bottom.ssd <- sum((pollutants$Bottom - bottom.mean) ^ 2)
bottom.ssd
```

Now using the values I just calculated I will update the posterior distribution parameters for the measurements recorded near the bottom.

```{r}
#Center
bottom.center <- ((prior.precision * prior.center) + (n * bottom.mean)) / (prior.precision + n)
#Precision
bottom.precision <- prior.precision + n
#shape
bottom.shape <- prior.shape + 1/2 * n
#scale
bottom.scale <- (1/2 * bottom.ssd + (prior.precision * n * (bottom.mean - prior.center) ^ 2) / (2 * (prior.precision + n))) ^ -1

bottom.params <- c(bottom.center, bottom.precision, bottom.shape, bottom.scale)

bottom.params
```

I then use the `dnormgam` function from the `NormalGamma` package to calculate the density over a range for $\theta_b$.

```{r}
bottom.density <- 
    data.frame(Mean = dnormgam(par = bottom.params, plot = F)$xout,
               Density = dnormgam(par = bottom.params, plot = F)$dout) %>%
    mutate(Depth = 'Bottom')
```

I follow the same steps for the measurements taken near the surface.
First I calculate the helpful metrics from the data.
```{r}
#Sample Mean
surface.mean <- sum(pollutants$Surface) / n
surface.mean
```
```{r}
#Sum of Squared Deviations
surface.ssd <- sum((pollutants$Surface - surface.mean) ^ 2)
surface.ssd
```

Then I update the posterior parameters.

```{r}
#Center
surface.center <- ((prior.precision * prior.center) + (n * surface.mean)) / (prior.precision + n)
#Precision
surface.precision <- prior.precision + n
#shape
surface.shape <- prior.shape + 1/2 * n
#scale
surface.scale <- (1/2 * surface.ssd + (prior.precision * n * (surface.mean - prior.center) ^ 2) / (2 * (prior.precision + n))) ^ -1

surface.params <- c(surface.center, surface.precision, surface.shape, surface.scale)

surface.params
```

Last I calculate the density and store in a `data.frame` in R.

```{r}
surface.density <- 
    data.frame(Mean = dnormgam(par = surface.params, plot = F)$xout,
               Density = dnormgam(par = surface.params, plot = F)$dout) %>%
    mutate(Depth = 'Surface')
```

Putting both densities together I can then visualize the joint posterior as the individual posteriors for the _bottom_ and _surface_ depth measurements.

```{r}
bind_rows(surface.density, bottom.density) %>%
    ggplot(aes(x = Mean,
               y = Density,
               color = Depth)) +
    geom_line(stat = 'identity') +
    labs(title = 'Joint Posterior Distributions') +
    theme_classic()
```