subplot(1,2,1)

boxplot(reward_data.peak_tanVel(reward_data.reward_type == 3));
ylim([min(reward_data.peak_tanVel) max(reward_data.peak_tanVel)])

subplot(1,2,2)
boxplot(reward_data.peak_tanVel(reward_data.reward_type == 4));
ylim([min(reward_data.peak_tanVel) max(reward_data.peak_tanVel)])

subplot(1,2,1)

boxplot(reward_data.peak_tanVel(reward_data.reward_type == 3 & reward_data.prep_time < 300 &  reward_data.prep_time > 50));
ylim([min(reward_data.peak_tanVel) max(reward_data.peak_tanVel)])

subplot(1,2,2)
boxplot(reward_data.peak_tanVel(reward_data.reward_type == 4 & reward_data.prep_time < 300 & reward_data.prep_time > 50));
ylim([min(reward_data.peak_tanVel) max(reward_data.peak_tanVel)]);

subplot(1,2,1)

boxplot(sum(reward_data.success(reward_data.reward_type == 3)));
ylim([100 125])

subplot(1,2,2)
boxplot(sum(reward_data.success(reward_data.reward_type == 4)));
ylim([100 125])

hold on
scatter(reward_data.eff_pt(reward_data.reward_type ==3), reward_data.peak_tanVel(reward_data.reward_type ==3));
scatter(reward_data.eff_pt(reward_data.reward_type ==4), reward_data.peak_tanVel(reward_data.reward_type ==4));

%%
subplot(1,2,1);
histogram(reward_data.eff_pt(reward_data.success == 0 & reward_data.reward_type == 3), 20)
xlim([-200 600]);
ylim([0 30]);
hold on
subplot(1,2,2);
histogram(reward_data.eff_pt(reward_data.success == 1 & reward_data.reward_type == 3),20)
xlim([-200 600]);
ylim([0 30]);


hold off

%histogram(reward_data.rt)

%%
bins = -200:100:600;

subplot(1,4,1);
histogram(tvns_data.eff_pt(tvns_data.success == 0 & tvns_data.stim_type == 1),'BinEdges', bins)
xlim([-200 600]);
ylim([0 60]);

subplot(1,4,2);
histogram(tvns_data.eff_pt(tvns_data.success == 1 & tvns_data.stim_type == 1),'BinEdges', bins)
xlim([-200 600]);
ylim([0 60]);
subplot(1,4,3);
histogram(tvns_data.eff_pt(tvns_data.success == 0 & tvns_data.stim_type == 2),'BinEdges', bins)
xlim([-200 600]);
ylim([0 60]); 
subplot(1,4,4);
histogram(tvns_data.eff_pt(tvns_data.success == 1 & tvns_data.stim_type == 2),'BinEdges', bins)
xlim([-200 600]);
ylim([0 60]);


hold off

%histogram(reward_data.rt)