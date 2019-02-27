package za.ac.sun.cs.coastal.observers;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.configuration2.ImmutableConfiguration;
import org.apache.logging.log4j.Logger;

import za.ac.sun.cs.coastal.COASTAL;
import za.ac.sun.cs.coastal.diver.SymbolicState;
import za.ac.sun.cs.coastal.messages.Broker;
import za.ac.sun.cs.coastal.messages.Tuple;
import za.ac.sun.cs.coastal.solver.Expression;

public class AutomataComparisonReporterFactory implements ObserverFactory {

	public AutomataComparisonReporterFactory(COASTAL coastal, ImmutableConfiguration options) {
	}

	@Override
	public int getFrequencyflags() {
		return ObserverFactory.ONCE_PER_TASK;
	}

	@Override
	public ObserverManager createManager(COASTAL coastal) {
		return new AutomataComparisonReporterManager(coastal);
	}

	@Override
	public Observer createObserver(COASTAL coastal, ObserverManager manager) {
		return new AutomataComparisonReporterObserver(coastal, manager);
	}

	// ======================================================================
	//
	// MANAGER FOR AUTOMATA COMPARISON
	//
	// ======================================================================

	private static class AutomataComparisonReporterManager implements ObserverManager {

		private final Broker broker;
		
		private final Map<Integer, Expression> conditions = new HashMap<>();
		
		private int invocationCount = 0;
		
		AutomataComparisonReporterManager(COASTAL coastal) {
			broker = coastal.getBroker();
			broker.subscribe("coastal-stop", this::report);
		}
		
		public synchronized void update(Boolean accepted, Expression pathCondition) {
			// Update the invocation count
			invocationCount += 1;
			
			// Store the new set of path conditions into the map
			conditions.put(invocationCount, pathCondition);
			
		}
		
		public void report(Object object) {
			broker.publish("path-condition-report", null);
			
			conditions.forEach((invocation, pc) -> broker.publish("report", new Tuple("AutoComp.[" + invocation + "] " + pc.toString())));
			
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
	// OBSERVER FOR AUTOMATA COMPARISON
	//
	// ======================================================================

	private static class AutomataComparisonReporterObserver implements Observer {

		private final Logger log;

		private final AutomataComparisonReporterManager manager;
		
		private final Broker broker;
		
		private boolean accepted = false;
		
		public AutomataComparisonReporterObserver(COASTAL coastal, ObserverManager manager) {
			log = coastal.getLog();
			this.manager = (AutomataComparisonReporterManager) manager;
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
			/* 0 represents the SUT, 1 represents hypothesis automaton */
			int source = (int) object;
			
			
		}

	}

}
