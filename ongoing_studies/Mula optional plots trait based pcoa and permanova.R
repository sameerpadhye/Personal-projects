

#functional diversity


#changing the rownames to match with the names of species in the cladocera abundance dataset

row.names(clado_mula_funtrait2) = colnames(cladocera_indicsp_mula[, -10])

View(clado_mula_funtrait2)

mula_fun_traits_all = dbFD(clado_mula_funtrait2, cladocera_indicsp_mula[, -10], w = c(2, 2, 1, 1, 1), corr = "cailliez", calc.FRic = T, calc.FDiv = T, ord = "podani", calc.FGR = T, clust.type = "average", stand.FRic = T, w.abun = F)
class(mula_fun_traits_all)

mula_fun_traits_all$CWM

#obtaining the Functional diversity values from the above dataset with the beta diversity grouping

fundiv_mula = as.data.frame(cbind(Fric = mula_fun_traits_all$FRic, Fdiv = mula_fun_traits_all$FDiv, Fdis = mula_fun_traits_all$FDis, Feve = mula_fun_traits_all$FEve, category = cladocera_all_mula3$Group))

fundiv_mula$cateogry = as.factor(fundiv_mula$category)
fundiv_mula = fundiv_mula[, -5]

View(fundiv_mula)

fundiv_mula = tibble::rownames_to_column(fundiv_mula, "samples")


##visualizing the Functional diversity by boxplots

fundiv_mula_plot = gather(fundiv_mula, Indices, Values, Fric:Feve)

fundiv_mula_plot$groups = as.factor(fundiv_mula_plot$groups)

#boxplot
gg_clado_plot1 = ggplot(fundiv_mula_plot, aes(x = groups, y = Values, fill = Indices)) + geom_boxplot() + theme_bw(base_size = 16)

gg_clado_plot1

# lineplot
ggplot(fundiv_mula, aes(x = samples, y = Feve, col = "category")) + geom_point() + theme_bw(base_size = 16)

#combining data for using both fun diversity and environment for PCA

clado_env_mula2 = cbind(clado_env_mula[, -2], fundiv_mula[, -1])

# Principal component analysis 

pca_clado2 <- prcomp(log10(clado_env_mula2[, -8] + 1), scale. = TRUE)

# Plot

autoplot(pca_clado2, loadings = TRUE, loadings.label = TRUE, data = clado_env_mula,, colour = 'group', label = F, loadings.label.size = 5, frame = T) + theme_bw()


######CLUSTERING USING TRAIT DATA###########

str(funcomp_clado_mula)

#categorical variable to numerical data

clad_env_fac = sapply(funcomp_clado_mula, is.factor)

mula_func_catvar = sapply(funcomp_clado_mula[clad_env_fac], unclass)
View(mula_func_catvar)

#numerical variables 

mula_func_contvar = select_if(funcomp_clado_mula, is.numeric)

#combining the transformed categorical variables and numerical variables

mula_func_numdata = as.data.frame(cbind(mula_func_catvar, mula_func_contvar, groups = cladocera_all_mula3$Group))

View(mula_func_numdata)

#beta diversity for the combined functional traits

clado_mula_beta_fun = vegdist(mula_func_numdata[-9], method = "gower")

#plot for visualization to assess the number of clusters 

plot_clado_mula_fun2 = hclust(clado_mula_beta_fun, method = "average")

plot(plot_clado_mula_fun2)

##beta dispersion of the functional trait groups

clado_mula_disper_fun = betadisper(clado_mula_beta_fun, mula_func_numdata$groups)

anova(clado_mula_disper_fun)

plot(clado_mula_disper_fun)

permutest(clado_mula_disper_fun, pairwise = TRUE)


#PERMANOVA to check the validity of the groups

clado_mula_perm_fun = adonis(mula_func_numdata[-9] ~ groups, data = mula_func_numdata, permutations = 9999, method = "gower")

clado_mula_perm_fun$aov.tab


##PLOTS FOR YEARWISE DISTRIBUTION OF FUNCTIONAL DIVERSITY VALUES (OPTIONAL)

##using the months and year columns from initial dataset
mula.months = clado_env_mula[, c(1, 2)]
View(clado_env_mula)
#joining the above dataset with the main dataset
all.data.mula2 = cbind(all.data.mula, mula.months)
View(all.data.mula2)
all.data.mula2$Month
#converting character into factor
all.data.mula2$Month = as.factor(all.data.mula2$Month)
all.data.mula2$Month
all.data.mula2$Year = as.factor(all.data.mula2$Year)
#re-adjusting the levels of Month factor
levels(all.data.mula2$Month) <- c("Jan", "Mar", 'May', 'Sept', 'Nov', 'Jan', 'Feb', 'Mar', 'April', 'May', 'June', 'Sept', 'Oct', 'Nov', 'Dec')

# yearwise plot with Fdis values 
ggplot(all.data.mula2, aes(Month, Feve, fill = Year, group = Year)) + geom_point(aes(fill = Year), pch = 21, size = 5, position = position_dodge(width = 0.4)) + geom_line(aes(colour = Year), size = 1, linetype = 'dashed') + theme_classic(base_size = 20) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = c("orange", "forestgreen")) + scale_color_manual(values = c("orange", "forestgreen"))

# yearwise plot with Fred values 
ggplot(all.data.mula2, aes(Month, Feve, fill = Year, group = Year)) + geom_point(aes(fill = Year), pch = 21, size = 5, position = position_dodge(width = 0.2)) + geom_line(aes(colour = Year), size = 1, linetype = 'dashed') + theme_classic(base_size = 20) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = c("orange", "forestgreen")) + scale_color_manual(values = c("orange", "forestgreen"))