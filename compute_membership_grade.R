compute_membership_grade <- function (x, c, std_dev) {
  membership_grade <- exp(-(x-c)^2/(2*std_dev^2))
  return (membership_grade)
}