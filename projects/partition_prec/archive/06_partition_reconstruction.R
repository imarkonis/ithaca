sample_pool <- elev_class_prec[rel_dataset_agreement == 'high', .(prec_mean = sample(prec_mean, 1000000, replace = TRUE)), .(elev_class)]
