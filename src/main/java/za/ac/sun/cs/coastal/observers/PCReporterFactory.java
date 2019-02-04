package za.ac.sun.cs.coastal.observers;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.configuration2.ImmutableConfiguration;
import org.apache.logging.log4j.Logger;

import za.ac.sun.cs.coastal.COASTAL;
import za.ac.sun.cs.coastal.diver.SegmentedPC;
import za.ac.sun.cs.coastal.diver.SymbolicState;
import za.ac.sun.cs.coastal.messages.Broker;
import za.ac.sun.cs.coastal.messages.Tuple;
import za.ac.sun.cs.coastal.solver.Expression;

public class PCReporterFactory implements ObserverFactory {

	public PCReporterFactory(COASTAL coastal, ImmutableConfiguration options) {
	}

	@Override
	public int getFrequencyflags() {
		return ObserverFactory.ONCE_PER_TASK;
	}

	@Override
	public ObserverManager createManager(COASTAL coastal) {
		return new PCReporterManager(coastal);
	}

	@Override
	public Observer createObserver(COASTAL coastal, ObserverManager manager) {
		return new PCReporterObserver(coastal, manager);
	}

	// ======================================================================
	//
	// MANAGER FOR PATH CONDITION REPORTING
	//
	// ======================================================================

	private static class PCReporterManager implements ObserverManager {

		private final Broker broker;
		
		private final Map<Boolean, HashSet<Expression>> conditions = new HashMap<>();
		
		PCReporterManager(COASTAL coastal) {
			broker = coastal.getBroker();
			broker.subscribe("coastal-stop", this::report);
		}

		public synchronized void update(Boolean accepted, Expression pathCondition) {
			HashSet<Expression> pathConditions = conditions.get(accepted);
			
			if (pathConditions == null) {
				pathConditions = new HashSet<>();
				pathConditions.add(pathCondition);
				conditions.put(accepted, pathConditions);
			} else {
				pathConditions.add(pathCondition);
				conditions.put(accepted, pathConditions);
			}
		}
		
		public void report(Object object) {
			broker.publish("path-condition-report", null);
			
			if (conditions.size() > 0) {
				for (Entry<Boolean, HashSet<Expression>> entry : conditions.entrySet()) {
					entry.getValue().forEach(cond -> broker.publish("report", new Tuple("PCReporter.pc[" + entry.getKey() + "]" + cond.toString())));
				}
			}
			
		}

		@Override
		public String getName() {
			return null;
		}

		@Override
		public String[] getPropertyNames() {
			return null;
		}

		@Override
		public Object[] getPropertyValues() {
			return null;
		}

	}

	// ======================================================================
	//
	// OBSERVER FOR MARKER COVERAGE
	//
	// ======================================================================

	private static class PCReporterObserver implements Observer {

		private final Logger log;

		private final PCReporterManager manager;
		
		private final Broker broker;
		
		private boolean accepted = false;

		PCReporterObserver(COASTAL coastal, ObserverManager manager) {
			log = coastal.getLog();
			this.manager = (PCReporterManager) manager;
			this.broker = coastal.getBroker();
			broker.subscribeThread("mark", this::mark);
			broker.subscribeThread("dive-end", this::diveEnd);
		}
		
		public void diveEnd(Object object) {
			Tuple result = (Tuple) object;
			SymbolicState state = (SymbolicState) result.get(1);
			
			manager.update(accepted, state.getSegmentedPathCondition().getPathCondition());
		}

		public void mark(Object object) {
			/* 1 represents acceptance, 0 represents failure */
			accepted = (((Integer) object) == 1) ? true : false;
		}

	}

}
