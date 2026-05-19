# --- MATCHING PENNIES --- 
# Setup
set.seed(1999)
library(tidyverse)
library(ggplot2)

# MATCHING PENNIES ASSIGNMENT
# Two cognitive strategies:
# Win-Stay-Lose-Shift with noise
# Exponential-memory learner


# OPPONENT STRATEGIES

random_opponent <- function(t, p_one = 0.5) {
  rbinom(1, 1, p_one)
}

biased_opponent <- function(t, p_one = 0.6) {
  rbinom(1, 1, p_one)
}

block_opponent <- function(t, block_size = 10) {
  
  block <- floor((t - 1) / block_size)
  
  if (block %% 2 == 0) {
    p <- 0.7
  } else {
    p <- 0.3
  }
  
  rbinom(1, 1, p)
}

# AGENT STRATEGY 1: WIN-STAY-LOSE-SHIFT WITH NOISE

wsls_choice_prob <- function(prev_choice, prev_feedback, noise = 0.1) {
  
  if (prev_feedback == 1) {
    deterministic_choice <- prev_choice
  } else {
    deterministic_choice <- 1 - prev_choice
  }
  
  p_choose_1 <- (1 - noise) * deterministic_choice + noise * 0.5
  
  return(p_choose_1)
}

# AGENT STRATEGY 2: EXPONENTIAL-MEMORY LEARNER


memory_choice_prob <- function(memory, beta = 0.8, bias = 0) {
  
  eta <- bias + beta * (2 * (memory - 0.5))
  p_choose_1 <- plogis(eta)
  
  return(p_choose_1)
}

update_memory <- function(memory, observed_other, alpha = 0.1) {
  
  new_memory <- memory + alpha * (observed_other - memory)
  
  return(new_memory)
}



# SIMULATE ONE AGENT


simulate_agent <- function(
    strategy = "wsls",
    opponent_type = "random",
    trials = 100,
    noise = 0.1,
    alpha = 0.2,
    beta = 2,
    bias = 0,
    m0 = 0.5,
    p_one = 0.6,
    block_size = 10
) {
  
  Self <- rep(NA, trials)
  Other <- rep(NA, trials)
  Feedback <- rep(NA, trials)
  P_self <- rep(NA, trials)
  Memory <- rep(NA, trials)
  
  memory_now <- m0
  
  for (t in 1:trials) {
    
    # Opponent choice
    if (opponent_type == "random") {
      Other[t] <- random_opponent(t, p_one = 0.5)
    }
    
    if (opponent_type == "biased") {
      Other[t] <- biased_opponent(t, p_one = p_one)
    }
    
    if (opponent_type == "block") {
      Other[t] <- block_opponent(t, block_size = block_size)
    }
    
    # Agent choice probability
    if (strategy == "wsls") {
      
      if (t == 1) {
        P_self[t] <- 0.5
      } else {
        P_self[t] <- wsls_choice_prob(
          prev_choice = Self[t - 1],
          prev_feedback = Feedback[t - 1],
          noise = noise
        )
      }
    }
    
    if (strategy == "memory") {
      
      Memory[t] <- memory_now
      
      P_self[t] <- memory_choice_prob(
        memory = memory_now,
        beta = beta,
        bias = bias
      )
    }
    
    # Realized choice
    Self[t] <- rbinom(1, 1, P_self[t])
    
    # Feedback -> success if agent matches opponent
    Feedback[t] <- as.integer(Self[t] == Other[t])
    
    # Memory update after observing opponent
    if (strategy == "memory") {
      memory_now <- update_memory(
        memory = memory_now,
        observed_other = Other[t],
        alpha = alpha
      )
    }
  }
  
  tibble(
    trial = 1:trials,
    strategy = strategy,
    opponent_type = opponent_type,
    Self = Self,
    Other = Other,
    Feedback = Feedback,
    P_self = P_self,
    Memory = Memory
  )
}


# SIMULATE MANY AGENTS


simulate_many_agents <- function(
    n_agents = 100,
    trials = 100,
    strategies = c("wsls", "memory"),
    opponent_types = c("random", "biased", "block")
) {
  
  all_data <- list()
  counter <- 1
  
  for (strategy in strategies) {
    for (opponent in opponent_types) {
      for (agent in 1:n_agents) {
        
        df <- simulate_agent(
          strategy = strategy,
          opponent_type = opponent,
          trials = trials
        ) %>%
          mutate(agent_id = agent)
        
        all_data[[counter]] <- df
        counter <- counter + 1
      }
    }
  }
  
  bind_rows(all_data)
}

df_all <- simulate_many_agents(
  n_agents = 100,
  trials = 100
)

# ADD PERFORMANCE MEASURES


df_all <- df_all %>%
  group_by(strategy, opponent_type, agent_id) %>%
  mutate(
    cumulative_success = cumsum(Feedback) / trial
  ) %>%
  ungroup()

summary_performance <- df_all %>%
  group_by(strategy, opponent_type, trial) %>%
  summarise(
    mean_success = mean(cumulative_success),
    lower = quantile(cumulative_success, 0.025),
    upper = quantile(cumulative_success, 0.975),
    .groups = "drop"
  )


# PLOTS


# Plot 1: Individual trajectories
ggplot(df_all, aes(trial, cumulative_success, group = agent_id)) +
  geom_line(alpha = 0.15) +
  facet_grid(strategy ~ opponent_type) +
  theme_classic() +
  labs(
    title = "Individual cumulative success rates",
    x = "Trial",
    y = "Cumulative success rate"
  )

# Plot 2: Mean performance with uncertainty interval
ggplot(summary_performance, aes(trial, mean_success)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1) +
  facet_grid(strategy ~ opponent_type) +
  theme_classic() +
  labs(
    title = "Average cumulative success by strategy and opponent",
    x = "Trial",
    y = "Mean cumulative success"
  )

# Plot 3: Final performance comparison
final_performance <- df_all %>%
  filter(trial == max(trial)) %>%
  group_by(strategy, opponent_type) %>%
  summarise(
    final_success = mean(cumulative_success),
    .groups = "drop"
  )

ggplot(final_performance, aes(opponent_type, final_success, fill = strategy)) +
  geom_col(position = "dodge") +
  theme_classic() +
  labs(
    title = "Final success rate after 100 trials",
    x = "Opponent type",
    y = "Final cumulative success",
    fill = "Strategy"
  )


# plots for the results 

# Mean cumulative success
ggplot(summary_performance, aes(trial, mean_success, color = strategy)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ opponent_type) +
  theme_classic() +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = strategy),
              alpha = 0.2,
              color = NA) +
  labs(
    title = "Mean cumulative success over time",
    x = "Trial",
    y = "Cumulative success rate",
    color = "Strategy"
  )


# Final success comparison
ggplot(final_performance, aes(opponent_type, final_success, fill = strategy)) +
  geom_col(position = "dodge") +
  theme_classic() +
  labs(
    title = "Final success rate after 100 trials",
    x = "Opponent type",
    y = "Final cumulative success",
    fill = "Strategy"
  )

# Memory trajectory
df_all %>%
  filter(strategy == "memory") %>%
  group_by(opponent_type, trial) %>%
  summarise(mean_memory = mean(Memory, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(trial, mean_memory)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ opponent_type) +
  theme_classic() +
  labs(
    title = "Memory trajectory of the exponential-memory learner",
    x = "Trial",
    y = "Mean memory estimate"
  )
