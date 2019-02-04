package za.ac.sun.cs.coastal.observers;

import java.util.HashMap;
import java.util.Map;

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
	// MANAGER FOR MARKER COVERAGE
	//
	// ======================================================================

	private static class PCReporterManager implements ObserverManager {

		private final Broker broker;

		private final Map<String, Integer> markers = new HashMap<>();
		
		private final Map<Integer, Expression> spcs = new HashMap<>();
		
		private int index = 0;

		PCReporterManager(COASTAL coastal) {
			broker = coastal.getBroker();
			broker.subscribe("coastal-stop", this::report);
		}

		public synchronized void update(Map<Integer, Expression> spcs) {
			for (Map.Entry<Integer, Expression> entry : spcs.entrySet()) {
				Integer idx = entry.getKey();
				Expression spc = entry.getValue();
				
				this.spcs.put(idx,  spc);
			}
		}
		
		public int getIndex() {
			return index++;
		}

		public void report(Object object) {
			broker.publish("path-condition-report", null);
			
			if (spcs.size() > 0) {
				for (Map.Entry<Integer, Expression> entry : spcs.entrySet()) {
					broker.publish("report",
							new Tuple("PCReporter.pc[" + entry.getKey() + "]", entry.getValue().toString()));
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

		private final Map<String, Integer> markers = new HashMap<>();
		
		private final Map<Integer, Expression> spcs = new HashMap<>();
		
		private final Broker broker;
		

		PCReporterObserver(COASTAL coastal, ObserverManager manager) {
			log = coastal.getLog();
			this.manager = (PCReporterManager) manager;
			this.broker = coastal.getBroker();
			broker.subscribe("marker-coverage-report", this::update);
			broker.subscribeThread("mark", this::mark);
			broker.subscribeThread("dive-end", this::diveEnd);
		}
		
		public void diveEnd(Object object) {
			Tuple result = (Tuple) object;
			SymbolicState state = (SymbolicState) result.get(1);
			
			Map<Integer, Expression> newSPC = new HashMap<>();
			newSPC.put(manager.getIndex(), state.getSegmentedPathCondition().getPathCondition());
			manager.update(newSPC);
		}

		public void mark(Object object) {
			String marker;
	
			if (object instanceof Integer) {
				marker = Integer.toString((Integer) object);
			} else {
				marker = (String) object;
			}
			log.debug("%%% mark hit {}", marker);
			Integer n = markers.get(marker);
			if (n == null) {
				markers.put(marker, 1);
			} else {
				markers.put(marker, n + 1);
			}
		}

		public void update(Object object) {
			manager.update(spcs);
		}

	}

}
